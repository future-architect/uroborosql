/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.client;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;

/**
 * 動的ロードしたDriverの中継用クラス<br>
 *
 * DriverManagerはclasspathで指定したクラスしか実行しないため、中継用のクラス経由で動的にロードしたDriverを実行する
 *
 * @author H.Sugimoto
 *
 */
public class DriverShim implements Driver {
	private final Driver driver;

	/**
	 * コンストラクタ
	 *
	 * @param driver 実Driver
	 */
	DriverShim(final Driver driver) {
		this.driver = driver;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#connect(java.lang.String, java.util.Properties)
	 */
	@Override
	public Connection connect(final String url, final Properties info) throws SQLException {
		return driver.connect(url, info);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#acceptsURL(java.lang.String)
	 */
	@Override
	public boolean acceptsURL(final String url) throws SQLException {
		return driver.acceptsURL(url);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#getPropertyInfo(java.lang.String, java.util.Properties)
	 */
	@Override
	public DriverPropertyInfo[] getPropertyInfo(final String url, final Properties info) throws SQLException {
		return driver.getPropertyInfo(url, info);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#getMajorVersion()
	 */
	@Override
	public int getMajorVersion() {
		return driver.getMajorVersion();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#getMinorVersion()
	 */
	@Override
	public int getMinorVersion() {
		return driver.getMinorVersion();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#jdbcCompliant()
	 */
	@Override
	public boolean jdbcCompliant() {
		return driver.jdbcCompliant();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.Driver#getParentLogger()
	 */
	@Override
	public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
		return driver.getParentLogger();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return driver.toString();
	}
}