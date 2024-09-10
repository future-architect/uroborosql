/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.io.BufferedInputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStore.SecretKeyEntry;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

import org.slf4j.Logger;

import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.BeforeSetParameterEvent;
import jp.co.future.uroborosql.log.EventLogger;
import jp.co.future.uroborosql.log.SettingLogger;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * 特定のカラムの読み書きに対して暗号化/復号化を行うイベントサブスクライバの抽象クラス.
 *
 * 登録、更新時はパラメータを暗号化 検索時は検索結果を復号化する
 *
 * @param <T> SecretColumnEventSubscriberの具象型
 * @author H.Sugimoto
 * @since v1.0.0
 *
 */
public abstract class AbstractSecretColumnEventSubscriber<T> extends EventSubscriber
		implements EventLogger, SettingLogger {
	/** イベントロガー */
	private static final Logger EVENT_LOG = EventLogger.getEventLogger("secretcolumn");

	/** 暗号キー */
	private SecretKey secretKey = null;

	/** 暗号器 */
	private Cipher encryptCipher = null;

	/** 秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。 */
	private String keyStoreFilePath = null;

	/** KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定する */
	private String storePassword = null;

	/** KeyStore内で秘密鍵が格納されている場所を示すエイリアス名 */
	private String alias = null;

	/** キャラクタセット（デフォルトUTF-8） */
	private Charset charset = StandardCharsets.UTF_8;

	/** 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する */
	private List<String> cryptColumnNames = null;

	/** 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存される */
	private List<String> cryptParamKeys = null;

	/** IVを利用するかどうか */
	private boolean useIV = false;

	private boolean skip = false;

	/**
	 * 変換の名前 (たとえば、DES/CBC/PKCS5Padding)。標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。
	 * 初期値は<code>AES/ECB/PKCS5Padding</code>
	 */
	private String transformationType = "AES/ECB/PKCS5Padding";

	protected AbstractSecretColumnEventSubscriber() {
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.subscriber.EventSubscriber#initialize()
	 */
	@Override
	public void initialize() {
		if (getCryptColumnNames() == null || getCryptColumnNames().isEmpty()) {
			setSkip(true);
			return;
		} else {
			cryptParamKeys = new ArrayList<>();
			var newColumnNames = new ArrayList<String>();
			for (var columnName : getCryptColumnNames()) {
				cryptParamKeys.add(CaseFormat.CAMEL_CASE.convert(columnName));
				newColumnNames.add(CaseFormat.UPPER_SNAKE_CASE.convert(columnName));
			}
			// 定義ファイルで指定されたカラム名は大文字でない可能性があるので、ここで大文字に置換し直す
			cryptColumnNames = newColumnNames;
		}

		KeyStore store;
		try {
			if (ObjectUtils.isBlank(getKeyStoreFilePath())) {
				atError(SETTING_LOG)
						.setMessage("Invalid KeyStore file path. Path:{}")
						.addArgument(getKeyStoreFilePath())
						.log();
				setSkip(true);
				return;
			}
			var storeFile = toPath(getKeyStoreFilePath());
			if (!Files.exists(storeFile)) {
				atError(SETTING_LOG)
						.setMessage("Not found KeyStore file path. Path:{}")
						.addArgument(getKeyStoreFilePath())
						.log();
				setSkip(true);
				return;
			}
			if (Files.isDirectory(storeFile)) {
				atError(SETTING_LOG)
						.setMessage("Invalid KeyStore file path. Path:{}")
						.addArgument(getKeyStoreFilePath())
						.log();
				setSkip(true);
				return;
			}
			if (ObjectUtils.isBlank(getStorePassword())) {
				atError(SETTING_LOG)
						.setMessage("Invalid password for access KeyStore.")
						.log();
				setSkip(true);
				return;
			}
			if (ObjectUtils.isBlank(getAlias())) {
				atError(SETTING_LOG)
						.setMessage("No alias for access KeyStore.")
						.log();
				setSkip(true);
				return;
			}

			store = KeyStore.getInstance("JCEKS");

			char[] pass;
			try (var is = new BufferedInputStream(Files.newInputStream(storeFile))) {
				pass = new String(Base64.getUrlDecoder().decode(getStorePassword())).toCharArray();

				store.load(is, pass);
			}

			var entry = (SecretKeyEntry) store.getEntry(getAlias(),
					new KeyStore.PasswordProtection(pass));

			secretKey = entry.getSecretKey();
			encryptCipher = Cipher.getInstance(transformationType);
			encryptCipher.init(Cipher.ENCRYPT_MODE, secretKey);
			useIV = encryptCipher.getIV() != null;
		} catch (Exception ex) {
			atError(SETTING_LOG)
					.setMessage("Failed to acquire secret key.")
					.setCause(ex)
					.log();
			setSkip(true);
		}

		beforeSetParameterListener(this::beforeSetParameter);
		afterSqlQueryListener(this::afterSqlQuery);
	}

	void beforeSetParameter(final BeforeSetParameterEvent evt) {
		// パラメータが暗号化対象のパラメータ名と一致する場合、パラメータの値を暗号化する
		var parameter = evt.getParameter();
		if (skip || parameter == null) {
			return;
		}

		if (Parameter.class.equals(parameter.getClass())) {
			// 通常のパラメータの場合
			var key = parameter.getParameterName();
			if (getCryptParamKeys().contains(CaseFormat.CAMEL_CASE.convert(key))) {
				var obj = parameter.getValue();
				if (obj != null) {
					String objStr = null;
					if (obj instanceof Optional) {
						objStr = ((Optional<?>) obj)
								.map(Object::toString)
								.orElse(null);
					} else {
						objStr = obj.toString();
					}
					if (ObjectUtils.isNotEmpty(objStr)) {
						try {
							synchronized (encryptCipher) {
								evt.setParameter(
										new Parameter(key, encrypt(encryptCipher, secretKey, objStr)));
							}
						} catch (Exception ex) {
							atWarn(EVENT_LOG)
									.setMessage("Encrypt Exception key:{}")
									.addArgument(key)
									.log();
						}
					}
				}
			}
		}

	}

	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		// 検索結果に暗号化対象カラムが含まれる場合、値の取得時に復号化されるようResultSetを SecretResultSet でラップして返す
		if (skip) {
			return;
		}

		try {
			evt.setResultSet(new SecretResultSet(evt.getResultSet(), this.createDecryptor(),
					getCryptColumnNames(), getCharset()));
		} catch (Exception ex) {
			atError(EVENT_LOG)
					.setMessage("Failed to create SecretResultSet.")
					.setCause(ex)
					.log();
		}
	}

	/**
	 * パラメータの暗号化内部処理。
	 *
	 * @param cipher 暗号器
	 * @param secretKey 暗号化キー
	 * @param input 暗号化対象文字列
	 * @return 暗号化後文字列
	 * @throws GeneralSecurityException サブクラスでの拡張に備えて javax.crypto パッケージ配下の例外の親クラス
	 */
	protected abstract String encrypt(final Cipher cipher, final SecretKey secretKey, final String input)
			throws GeneralSecurityException;

	/**
	 * パラメータの復号化内部処理。
	 *
	 * @param cipher 暗号器
	 * @param secretKey 暗号化キー
	 * @param secret 暗号化文字列
	 * @return 平文文字列
	 * @throws GeneralSecurityException サブクラスでの拡張に備えて javax.crypto パッケージ配下の例外の親クラス
	 */
	protected abstract String decrypt(final Cipher cipher, final SecretKey secretKey, final String secret)
			throws GeneralSecurityException;

	/**
	 * {@link SecretResultSet} が復号に使用するラムダを構築する。
	 *
	 * @return 暗号を受け取り平文を返すラムダ
	 * @throws GeneralSecurityException サブクラスでの拡張に備えて javax.crypto パッケージ配下の例外の親クラス
	 */
	private Function<Object, String> createDecryptor() throws GeneralSecurityException {
		var cipher = Cipher.getInstance(transformationType);
		if (useIV) {
			cipher.init(Cipher.DECRYPT_MODE, secretKey,
					encryptCipher.getParameters().getParameterSpec(IvParameterSpec.class));
		} else {
			cipher.init(Cipher.DECRYPT_MODE, secretKey);
		}

		return secret -> {
			if (secret == null) {
				return null;
			}

			var secretStr = secret.toString();
			if (ObjectUtils.isNotEmpty(secretStr)) {
				synchronized (cipher) {
					try {
						return decrypt(cipher, secretKey, secretStr);
					} catch (Exception ex) {
						return secretStr;
					}
				}
			} else {
				return secretStr;
			}
		};
	}

	/**
	 * 文字列からPathオブジェクトに変換する。 Pathの取得方法をカスタマイズしたい場合にオーバーライドする。
	 *
	 * @param path パス文字列
	 * @return Pathオブジェクト
	 */
	protected Path toPath(final String path) {
		return Paths.get(path);
	}

	/**
	 * 秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。を取得します。
	 *
	 * @return 秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。
	 */
	public String getKeyStoreFilePath() {
		return keyStoreFilePath;
	}

	/**
	 * 秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。を設定します。
	 *
	 * @param keyStoreFilePath 秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setKeyStoreFilePath(final String keyStoreFilePath) {
		this.keyStoreFilePath = keyStoreFilePath;
		return (T) this;
	}

	/**
	 * KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定するを取得します。
	 *
	 * @return KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定する
	 */
	public String getStorePassword() {
		return storePassword;
	}

	/**
	 * KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定するを設定します。
	 *
	 * @param storePassword KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定する
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setStorePassword(final String storePassword) {
		this.storePassword = storePassword;
		return (T) this;
	}

	/**
	 * KeyStore内で秘密鍵が格納されている場所を示すエイリアス名を取得します。
	 *
	 * @return KeyStore内で秘密鍵が格納されている場所を示すエイリアス名
	 */
	public String getAlias() {
		return alias;
	}

	/**
	 * KeyStore内で秘密鍵が格納されている場所を示すエイリアス名を設定します。
	 *
	 * @param alias KeyStore内で秘密鍵が格納されている場所を示すエイリアス名
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setAlias(final String alias) {
		this.alias = alias;
		return (T) this;
	}

	/**
	 * キャラクタセット（デフォルトUTF-8）を取得します。
	 *
	 * @return キャラクタセット（デフォルトUTF-8）
	 */
	public Charset getCharset() {
		return charset;
	}

	/**
	 * キャラクタセット（デフォルトUTF-8）を設定します。
	 *
	 * @param charset キャラクタセット（デフォルトUTF-8）
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setCharset(final String charset) {
		try {
			this.charset = Charset.forName(charset);
		} catch (UnsupportedCharsetException ex) {
			this.charset = StandardCharsets.UTF_8;
			atError(SETTING_LOG)
					.setMessage(
							"The specified character set could not be converted to {}. Set the default character set({}).")
					.addArgument(charset)
					.addArgument(this.charset)
					.log();
		}
		return (T) this;
	}

	/**
	 * 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定するを取得します。
	 *
	 * @return 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する
	 */
	public List<String> getCryptColumnNames() {
		return cryptColumnNames;
	}

	/**
	 * 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定するを設定します。
	 *
	 * @param cryptColumnNames 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setCryptColumnNames(final List<String> cryptColumnNames) {
		this.cryptColumnNames = cryptColumnNames;
		return (T) this;
	}

	/**
	 * 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存されるを取得します。
	 *
	 * @return 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存される
	 */
	protected List<String> getCryptParamKeys() {
		return cryptParamKeys;
	}

	/**
	 * 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存されるを設定します。
	 *
	 * @param cryptParamKeys 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存される
	 */
	protected void setCryptParamKeys(final List<String> cryptParamKeys) {
		this.cryptParamKeys = cryptParamKeys;
	}

	/**
	 * skipを取得します。
	 *
	 * @return skip
	 */
	public boolean isSkip() {
		return skip;
	}

	/**
	 * skipを設定します。
	 *
	 * @param skip skip
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setSkip(final boolean skip) {
		this.skip = skip;
		return (T) this;
	}

	/**
	 * 暗号キーを取得します。
	 *
	 * @return 暗号キー
	 */
	protected SecretKey getSecretKey() {
		return secretKey;
	}

	/**
	 * IVを利用するかどうかを取得します。
	 *
	 * @return IVを利用するかどうか
	 */
	protected boolean isUseIV() {
		return useIV;
	}

	/**
	 * 変換の名前を取得する 標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。 初期値は<code>AES/ECB/PKCS5Padding</code>
	 *
	 * @return 変換の名前
	 */
	public String getTransformationType() {
		return transformationType;
	}

	/**
	 * 変換の名前を設定する 標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。 初期値は<code>AES/ECB/PKCS5Padding</code>
	 *
	 * @param transformationType 変換の名前
	 * @return 具象型のインスタンス
	 */
	@SuppressWarnings("unchecked")
	public T setTransformationType(final String transformationType) {
		this.transformationType = transformationType;
		return (T) this;
	}

}