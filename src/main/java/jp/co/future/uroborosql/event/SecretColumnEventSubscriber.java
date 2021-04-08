/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.security.GeneralSecurityException;
import java.util.Arrays;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

/**
 * 特定のカラムの読み書きに対して暗号化/復号化を行うイベントサブスクライバのデフォルト実装.<br>
 * 登録、更新時はパラメータを暗号化 検索時は検索結果を復号化する.
 *
 * @author yanagihara
 */
public class SecretColumnEventSubscriber extends AbstractSecretColumnEventSubscriber {

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String encrypt(final Cipher cipher, final SecretKey secretKey, final String input)
			throws GeneralSecurityException {
		byte[] crypted;
		if (isUseIV()) {
			cipher.init(Cipher.ENCRYPT_MODE, secretKey);
		}
		crypted = cipher.doFinal(input.getBytes(getCharset()));
		if (isUseIV()) {
			var ivArray = cipher.getIV();
			var cryptedArray = crypted;
			crypted = new byte[ivArray.length + cryptedArray.length];
			System.arraycopy(ivArray, 0, crypted, 0, ivArray.length);
			System.arraycopy(cryptedArray, 0, crypted, ivArray.length, cryptedArray.length);
		}
		return Base64.getUrlEncoder().withoutPadding().encodeToString(crypted);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected String decrypt(final Cipher cipher, final SecretKey secretKey, final String secret)
			throws GeneralSecurityException {
		var secretData = Base64.getUrlDecoder().decode(secret);

		if (isUseIV()) {
			var blockSize = cipher.getBlockSize();
			var iv = Arrays.copyOfRange(secretData, 0, blockSize);
			secretData = Arrays.copyOfRange(secretData, blockSize, secretData.length);
			var ips = new IvParameterSpec(iv);
			cipher.init(Cipher.DECRYPT_MODE, secretKey, ips);
		}
		return new String(cipher.doFinal(secretData), getCharset());
	}
}
