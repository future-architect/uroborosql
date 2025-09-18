/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Tableインターフェースのテストクラス
 */
public class TableTest {

	/**
	 * テスト用のTable実装
	 */
	private static class TestTable implements Table {
		private final String name;
		private final String schema;

		public TestTable(String name, String schema) {
			this.name = name;
			this.schema = schema;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public String getSchema() {
			return schema;
		}
	}

	@Test
	void testGetIdentifier_WithoutSchema() {
		Table table = new TestTable("test_table", null);
		assertThat(table.getIdentifier(), is("test_table"));
	}

	@Test
	void testGetIdentifier_WithEmptySchema() {
		Table table = new TestTable("test_table", "");
		assertThat(table.getIdentifier(), is("test_table"));
	}

	@Test
	void testGetIdentifier_WithSchema() {
		Table table = new TestTable("test_table", "test_schema");
		assertThat(table.getIdentifier(), is("test_schema.test_table"));
	}

	@Test
	void testGetIdentifier_WithWhitespaceSchema() {
		Table table = new TestTable("test_table", "   ");
		assertThat(table.getIdentifier(), is("   .test_table"));
	}
}