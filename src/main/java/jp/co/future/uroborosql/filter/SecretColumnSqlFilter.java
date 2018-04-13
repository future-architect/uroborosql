package jp.co.future.uroborosql.filter;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStore;
import java.security.KeyStore.SecretKeyEntry;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 特定のカラムの読み書きに対して暗号化/復号化を行うSQLフィルター.
 *
 * 登録、更新時はパラメータを暗号化 検索時は検索結果を復号化する
 *
 * @author H.Sugimoto
 *
 */
public class SecretColumnSqlFilter extends AbstractSqlFilter {
	private static final Logger LOG = LoggerFactory.getLogger(SecretColumnSqlFilter.class);

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

	private boolean skipFilter = false;

	/**
	 * 変換の名前 (たとえば、DES/CBC/PKCS5Padding)。標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。
	 * 初期値は<code>AES/ECB/PKCS5Padding</code>
	 */
	private String transformationType = "AES/ECB/PKCS5Padding";

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
			Path storeFile = toPath(getKeyStoreFilePath());
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

			KeyStore.SecretKeyEntry entry = (SecretKeyEntry) store.getEntry(getAlias(),
					new KeyStore.PasswordProtection(pass));

			secretKey = entry.getSecretKey();
			encryptCipher = Cipher.getInstance(transformationType);
			encryptCipher.init(Cipher.ENCRYPT_MODE, secretKey);
		} catch (Exception ex) {
			LOG.error("Failed to acquire secret key. Cause:{}", ex.getMessage());
			setSkipFilter(true);
			ex.printStackTrace();
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
			String key = parameter.getParameterName();
			if (getCryptParamKeys().contains(CaseFormat.CAMEL_CASE.convert(key))) {
				Object obj = parameter.getValue();
				if (obj != null && obj instanceof String) {
					String objStr = obj.toString();
					if (StringUtils.isNotEmpty(objStr)) {
						try {
							synchronized (encryptCipher) {
								byte[] crypted = encryptCipher.doFinal(StringUtils.defaultString(objStr).getBytes(
										getCharset()));
								return new Parameter(key, Base64.getUrlEncoder().withoutPadding()
										.encodeToString(crypted));
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
	 * {@inheritDoc}
	 *
	 * 検索結果に暗号化対象カラムが含まれる場合、値の取得時に復号化されるようResultSetを{@link SecretResultSet}でラップして返す
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		if (skipFilter) {
			return resultSet;
		}

		try {
			Cipher cipher = Cipher.getInstance(transformationType);
			cipher.init(Cipher.DECRYPT_MODE, secretKey);

			return new SecretResultSet(resultSet, cipher, getCryptColumnNames(), getCharset());
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return resultSet;
	}

	/**
	 * 文字列からPathオブジェクトに変換する。
	 * Pathの取得方法をカスタマイズしたい場合にオーバーライドする。
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
	 * @param keyStoreFilePath
	 *            秘密鍵を格納したKeyStoreファイルのパス. KeyStoreはJCEKSタイプであること。
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
	 * @param storePassword
	 *            KeyStoreにアクセスするためのストアパスワード. Base64エンコードした値を指定する
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
	 * @param alias
	 *            KeyStore内で秘密鍵が格納されている場所を示すエイリアス名
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
	 * @param cryptColumnNames
	 *            暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する
	 */
	public void setCryptColumnNames(final List<String> cryptColumnNames) {
		this.cryptColumnNames = cryptColumnNames;
	}

	/**
	 * 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存されるを取得します。
	 *
	 * @return 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存される
	 */
	public List<String> getCryptParamKeys() {
		return cryptParamKeys;
	}

	/**
	 * 暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存されるを設定します。
	 *
	 * @param cryptParamKeys
	 *            暗号化、復号化を行うパラメータ名リスト. キャメルケースで保存される
	 */
	public void setCryptParamKeys(final List<String> cryptParamKeys) {
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
	 * @param skipFilter
	 *            skipFilter
	 */
	public void setSkipFilter(final boolean skipFilter) {
		this.skipFilter = skipFilter;
	}

	/**
	 * 変換の名前を取得する
	 * 標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。
	 * 初期値は<code>AES/ECB/PKCS5Padding</code>
	 *
	 * @return 変換の名前
	 */
	public String getTransformationType() {
		return transformationType;
	}

	/**
	 * 変換の名前を設定する
	 * 標準の変換名については、Java 暗号化アーキテクチャー標準アルゴリズム名のドキュメントの Cipher のセクションを参照。
	 * 初期値は<code>AES/ECB/PKCS5Padding</code>
	 *
	 * @param transformationType 変換の名前
	 */
	public void setTransformationType(final String transformationType) {
		this.transformationType = transformationType;
	}
}
