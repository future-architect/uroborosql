/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStore.SecretKeyEntry;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * 特定のカラムの読み書きに対して暗号化/復号化を行うSQLフィルターの抽象クラス.
 *
 * 登録、更新時はパラメータを暗号化 検索時は検索結果を復号化する
 *
 * @author H.Sugimoto
 *
 */
public abstract class AbstractSecretColumnSqlFilter extends AbstractSqlFilter {

	private static final Logger LOG = LoggerFactory.getLogger(AbstractSecretColumnSqlFilter.class);

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

	private boolean skipFilter = false;

	/**
	 * 変換の名前 (たとえば、DES/CBC/PKCS5Padding)。標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。
	 * 初期値は<code>AES/ECB/PKCS5Padding</code>
	 */
	private String transformationType = "AES/ECB/PKCS5Padding";

	public AbstractSecretColumnSqlFilter() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#initialize()
	 */
	@Override
	public void initialize() {
		if (getCryptColumnNames() == null || getCryptColumnNames().isEmpty()) {
			setSkipFilter(true);
			return;
		} else {
			cryptParamKeys = new ArrayList<>();
			List<String> newColumnNames = new ArrayList<>();
			for (String columnName : getCryptColumnNames()) {
				cryptParamKeys.add(CaseFormat.CAMEL_CASE.convert(columnName));
				newColumnNames.add(CaseFormat.UPPER_SNAKE_CASE.convert(columnName));
			}
			// 定義ファイルで指定されたカラム名は大文字でない可能性があるので、ここで大文字に置換し直す
			cryptColumnNames = newColumnNames;
		}

		KeyStore store;
		try {
			if (StringUtils.isBlank(getKeyStoreFilePath())) {
				LOG.error("Invalid KeyStore file path. Path:{}", getKeyStoreFilePath());
				setSkipFilter(true);
				return;
			}
			var storeFile = toPath(getKeyStoreFilePath());
			if (!Files.exists(storeFile)) {
				LOG.error("Not found KeyStore file path. Path:{}", getKeyStoreFilePath());
				setSkipFilter(true);
				return;
			}
			if (Files.isDirectory(storeFile)) {
				LOG.error("Invalid KeyStore file path. Path:{}", getKeyStoreFilePath());
				setSkipFilter(true);
				return;
			}
			if (StringUtils.isBlank(getStorePassword())) {
				LOG.error("Invalid password for access KeyStore.");
				setSkipFilter(true);
				return;
			}
			if (StringUtils.isBlank(getAlias())) {
				LOG.error("No alias for access KeyStore.");
				setSkipFilter(true);
				return;
			}

			store = KeyStore.getInstance("JCEKS");

			char[] pass;
			try (InputStream is = new BufferedInputStream(Files.newInputStream(storeFile))) {
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
			LOG.error("Failed to acquire secret key.", ex);
			setSkipFilter(true);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * パラメータが暗号化対象のパラメータ名と一致する場合、パラメータの値を暗号化する
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doParameter(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public Parameter doParameter(final Parameter parameter) {
		if (skipFilter || parameter == null) {
			return parameter;
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
					if (StringUtils.isNotEmpty(objStr)) {
						try {
							synchronized (encryptCipher) {
								return new Parameter(key, encrypt(encryptCipher, secretKey, objStr));
							}
						} catch (Exception ex) {
							return parameter;
						}
					}
				}
			}
		}

		return parameter;
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
	 * {@inheritDoc}
	 *
	 * 検索結果に暗号化対象カラムが含まれる場合、値の取得時に復号化されるようResultSetを{@link SecretResultSet}でラップして返す
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.ExecutionContext,
	 *      java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final ExecutionContext executionContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		if (skipFilter) {
			return resultSet;
		}

		try {
			return new SecretResultSet(resultSet, this.createDecryptor(), getCryptColumnNames(), getCharset());
		} catch (Exception ex) {
			LOG.error("Failed to create SecretResultSet.", ex);
		}
		return resultSet;
	}

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
			if (StringUtils.isNotEmpty(secretStr)) {
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
	 */
	public void setKeyStoreFilePath(final String keyStoreFilePath) {
		this.keyStoreFilePath = keyStoreFilePath;
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
	 */
	public void setStorePassword(final String storePassword) {
		this.storePassword = storePassword;
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
	 */
	public void setAlias(final String alias) {
		this.alias = alias;
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
	 */
	public void setCharset(final String charset) {
		try {
			this.charset = Charset.forName(charset);
		} catch (UnsupportedCharsetException ex) {
			this.charset = StandardCharsets.UTF_8;
			LOG.error("The specified character set could not be converted to {}. Set the default character set({}).",
					charset, this.charset);
		}
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
	 */
	public void setCryptColumnNames(final List<String> cryptColumnNames) {
		this.cryptColumnNames = cryptColumnNames;
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
	 * skipFilterを取得します。
	 *
	 * @return skipFilter
	 */
	public boolean isSkipFilter() {
		return skipFilter;
	}

	/**
	 * skipFilterを設定します。
	 *
	 * @param skipFilter skipFilter
	 */
	public void setSkipFilter(final boolean skipFilter) {
		this.skipFilter = skipFilter;
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
	 */
	public void setTransformationType(final String transformationType) {
		this.transformationType = transformationType;
	}

}