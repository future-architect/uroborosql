package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

public class MappingUtilsTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:MappingUtilsTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute("drop schema if exists schema1");
				stmt.execute("create schema if not exists schema1");
				stmt.execute("drop schema if exists schema2");
				stmt.execute("create schema if not exists schema2");

				stmt.execute("set schema schema1");
				stmt.execute("drop table if exists test");
				stmt.execute("create table if not exists test( id1 NUMERIC(4), name1 VARCHAR(10) )");

				stmt.execute("set schema schema2");
				stmt.execute("drop table if exists test");
				stmt.execute("create table if not exists test( id2 NUMERIC(4), name2 VARCHAR(10) )");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			//			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Test
	public void testMultiSchema() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();

				assertThat(agent.queryWith("select schema() as current_schema").one().get("CURRENT_SCHEMA"),
						is("SCHEMA1"));

				var test1 = new Test1(1, "name1");
				agent.insert(test1);
				var test1Result = agent.query(Test1.class).first().orElse(null);
				assertThat(test1Result.getId1(), is(test1.getId1()));
				assertThat(test1Result.getName1(), is(test1.getName1()));

				agent.updateWith("set schema schema2").count();

				assertThat(agent.queryWith("select schema() as current_schema").one().get("CURRENT_SCHEMA"),
						is("SCHEMA2"));

				var test2 = new Test2(1, "name2");
				agent.insert(test2);
				var test2Result = agent.query(Test2.class).first().orElse(null);
				assertThat(test2Result.getId2(), is(test2.getId2()));
				assertThat(test2Result.getName2(), is(test2.getName2()));
			});
		}
	}

	@Test
	public void testGetMappingColumn() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();
				assertThat(MappingUtils.getMappingColumn(Test1.class, "id1").getName(), is("ID1"));
				assertThat(MappingUtils.getMappingColumn(Test1.class, SqlKind.NONE, "id1").getName(), is("ID1"));
				assertThat(MappingUtils.getMappingColumn("SCHEMA1", Test1.class, "id1").getName(), is("ID1"));
				agent.updateWith("set schema schema2").count();
				assertThat(MappingUtils.getMappingColumn(Test1.class, "id1").getName(), is("ID1"));
				assertThat(MappingUtils.getMappingColumn(Test1.class, SqlKind.NONE, "id1").getName(), is("ID1"));
				assertThat(MappingUtils.getMappingColumn("SCHEMA2", Test1.class, "id1").getName(), is("ID1"));
			});
		}
	}

	@Test
	public void testGetMappingColumns() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();
				var test1Columns = MappingUtils.getMappingColumns(Test1.class);
				assertThat(test1Columns.length, is(3));
				test1Columns = MappingUtils.getMappingColumns(Test1.class, SqlKind.NONE);
				assertThat(test1Columns.length, is(3));

				agent.updateWith("set schema schema2").count();
				var test2Columns = MappingUtils.getMappingColumns(Test1.class);
				assertThat(test2Columns.length, is(3));
				test2Columns = MappingUtils.getMappingColumns(Test1.class, SqlKind.NONE);
				assertThat(test2Columns.length, is(3));
			});
		}
	}

	@Test
	public void testGetMappingColumnMap() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();
				var test1Columns = MappingUtils.getMappingColumnMap(Test1.class, SqlKind.NONE);
				assertThat(test1Columns.size(), is(3));
				test1Columns = MappingUtils.getMappingColumnMap("SCHEMA1", Test1.class, SqlKind.NONE);
				assertThat(test1Columns.size(), is(3));

				agent.updateWith("set schema schema2").count();
				var test2Columns = MappingUtils.getMappingColumnMap(Test1.class, SqlKind.NONE);
				assertThat(test2Columns.size(), is(3));
				test2Columns = MappingUtils.getMappingColumnMap("SCHEMA2", Test1.class, SqlKind.NONE);
				assertThat(test2Columns.size(), is(3));
			});
		}
	}

	@Test
	public void testGetIdMappingColumns() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();
				var test1IdColumns = MappingUtils.getIdMappingColumns(Test1.class);
				assertThat(test1IdColumns.length, is(1));
				assertThat(test1IdColumns[0].getName(), is("ID1"));
				var test1IdColumnsWithSchema = MappingUtils.getIdMappingColumns("SCHEMA1", Test1.class);
				assertThat(test1IdColumnsWithSchema.length, is(1));
				assertThat(test1IdColumnsWithSchema[0].getName(), is("ID1"));

				agent.updateWith("set schema schema2").count();
				var test2IdColumns = MappingUtils.getIdMappingColumns(Test1.class);
				assertThat(test2IdColumns.length, is(1));
				assertThat(test2IdColumns[0].getName(), is("ID1"));
				var test2IdColumnsWithSchema = MappingUtils.getIdMappingColumns("SCHEMA2", Test1.class);
				assertThat(test2IdColumnsWithSchema.length, is(1));
				assertThat(test2IdColumnsWithSchema[0].getName(), is("ID1"));

			});
		}
	}

	@Test
	public void testGetVersionMappingColumn() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("set schema schema1").count();
				var test1VersionColumn = MappingUtils.getVersionMappingColumn(Test1.class);
				assertThat(test1VersionColumn.isPresent(), is(true));
				test1VersionColumn = MappingUtils.getVersionMappingColumn("SCHEMA1", Test1.class);
				assertThat(test1VersionColumn.isPresent(), is(true));

				agent.updateWith("set schema schema2").count();
				var test2VersionColumn = MappingUtils.getVersionMappingColumn(Test1.class);
				assertThat(test2VersionColumn.isPresent(), is(true));
				test2VersionColumn = MappingUtils.getVersionMappingColumn("SCHEMA2", Test1.class);
				assertThat(test2VersionColumn.isPresent(), is(true));

				test2VersionColumn = MappingUtils.getVersionMappingColumn(Test2.class);
				assertThat(test2VersionColumn.isPresent(), is(false));
				test2VersionColumn = MappingUtils.getVersionMappingColumn("SCHEMA2", Test2.class);
				assertThat(test2VersionColumn.isPresent(), is(false));
			});
		}
	}

	@Test
	public void testClearCache() throws Exception {
		MappingUtils.clearCache();
	}

	@Table(name = "TEST")
	public static class Test1 {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private Integer id1;
		private String name1;
		@Version
		private Integer version;

		public Test1() {
		}

		public Test1(final Integer id1, final String name1) {
			this.id1 = id1;
			this.name1 = name1;
		}

		public Integer getId1() {
			return id1;
		}

		public void setId1(final Integer id1) {
			this.id1 = id1;
		}

		public String getName1() {
			return name1;
		}

		public void setName1(final String name1) {
			this.name1 = name1;
		}
	}

	@Table(name = "TEST")
	public static class Test2 {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private Integer id2;
		private String name2;

		public Test2() {
		}

		public Test2(final Integer id2, final String name2) {
			this.id2 = id2;
			this.name2 = name2;
		}

		public Integer getId2() {
			return id2;
		}

		public void setId2(final Integer id2) {
			this.id2 = id2;
		}

		public String getName2() {
			return name2;
		}

		public void setName2(final String name2) {
			this.name2 = name2;
		}
	}
}
