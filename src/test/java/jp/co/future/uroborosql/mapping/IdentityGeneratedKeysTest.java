package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class IdentityGeneratedKeysTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:IdentityGeneratedKeysTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");
				stmt.execute("drop sequence if exists test_id_seq");
				stmt.execute("create sequence test_id_seq");
				stmt.execute(
						"create table if not exists test( id bigint not null default nextval('test_id_seq'), name text, primary key(id))");

				stmt.execute("drop table if exists test_auto cascade");
				stmt.execute(
						"create table if not exists test_auto( id bigint auto_increment, name text, primary key(id))");

				stmt.execute("drop table if exists test_multikey");
				stmt.execute("drop sequence if exists test_multikey_id_seq");
				stmt.execute("create sequence test_multikey_id_seq");
				stmt.execute("drop sequence if exists test_multikey_id2_seq");
				stmt.execute("create sequence test_multikey_id2_seq start with 100");
				stmt.execute(
						"create table if not exists test_multikey( id bigint not null default nextval('test_multikey_id_seq'), id2 bigint not null default nextval('test_multikey_id2_seq'), name text, primary key(id, id2))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.updateWith("delete from test_multikey").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithId test1 = new TestEntityWithId("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestEntityWithId test2 = new TestEntityWithId("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++currVal));

				TestEntityWithId test3 = new TestEntityWithId("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertWithNoId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				TestAutoEntityWithNoId test1 = new TestAutoEntityWithNoId("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestAutoEntityWithNoId test2 = new TestAutoEntityWithNoId("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++currVal));

				TestAutoEntityWithNoId test3 = new TestAutoEntityWithNoId("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++currVal));

				TestAutoEntityWithNoId data = agent.find(TestAutoEntityWithNoId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestAutoEntityWithNoId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestAutoEntityWithNoId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertWithNoIdObj() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				long idVal = currVal + 100;

				TestAutoEntityWithNoIdObj test1 = new TestAutoEntityWithNoIdObj("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestAutoEntityWithNoIdObj test2 = new TestAutoEntityWithNoIdObj("name2");
				test2.id = idVal;
				agent.insert(test2);
				assertThat(test2.getId(), is(idVal));

				TestAutoEntityWithNoIdObj data = agent.find(TestAutoEntityWithNoIdObj.class, test1.getId())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestAutoEntityWithNoIdObj.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
			});
		}
	}

	@Test
	public void testInsertWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithId test1 = new TestEntityWithId("name1");
				test1.setId(100);
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));
				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void testInsertWithObjectKeyValueNotSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));
				TestEntityWithIdObj data = agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void testInsertWithObjectKeyValueSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long id = currVal + 100;

				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				test1.setId(id);
				agent.insert(test1);
				assertThat(test1.getId(), is(id));
				TestEntityWithIdObj data = agent.find(TestEntityWithIdObj.class, id).orElse(null);
				assertThat(data, is(test1));
			});
		}
	}

	@Test
	public void testInserts() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestEntityWithId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsNoId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				List<TestAutoEntityWithNoId> entites = new ArrayList<>();
				TestAutoEntityWithNoId test1 = new TestAutoEntityWithNoId("name1");
				entites.add(test1);

				TestAutoEntityWithNoId test2 = new TestAutoEntityWithNoId("name2");
				entites.add(test2);

				TestAutoEntityWithNoId test3 = new TestAutoEntityWithNoId("name3");
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestAutoEntityWithNoId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsNoIdObjNotSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				List<TestAutoEntityWithNoIdObj> entites = new ArrayList<>();
				TestAutoEntityWithNoIdObj test1 = new TestAutoEntityWithNoIdObj("name1");
				entites.add(test1);

				TestAutoEntityWithNoIdObj test2 = new TestAutoEntityWithNoIdObj("name2");
				entites.add(test2);

				TestAutoEntityWithNoIdObj test3 = new TestAutoEntityWithNoIdObj("name3");
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsNoIdObjSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				long idVal = currVal + 100;

				List<TestAutoEntityWithNoIdObj> entites = new ArrayList<>();
				TestAutoEntityWithNoIdObj test1 = new TestAutoEntityWithNoIdObj("name1");
				test1.id = idVal + 1;
				entites.add(test1);

				TestAutoEntityWithNoIdObj test2 = new TestAutoEntityWithNoIdObj("name2");
				test2.id = idVal + 2;
				entites.add(test2);

				TestAutoEntityWithNoIdObj test3 = new TestAutoEntityWithNoIdObj("name3");
				test3.id = idVal + 3;
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test3.getId(), is(idVal + 3));

				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				test1.id = idVal++;
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				test2.id = idVal++;
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				test3.id = idVal++;
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestEntityWithId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsWithObjectKeyValueNotSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				List<TestEntityWithIdObj> entites = new ArrayList<>();
				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				entites.add(test1);

				TestEntityWithIdObj test2 = new TestEntityWithIdObj("name2");
				entites.add(test2);

				TestEntityWithIdObj test3 = new TestEntityWithIdObj("name3");
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsWithObjectKeyValueSetValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				List<TestEntityWithIdObj> entites = new ArrayList<>();
				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				test1.id = idVal;
				entites.add(test1);

				TestEntityWithIdObj test2 = new TestEntityWithIdObj("name2");
				test2.id = idVal + 1;
				entites.add(test2);

				TestEntityWithIdObj test3 = new TestEntityWithIdObj("name3");
				test3.id = idVal + 2;
				entites.add(test3);

				agent.inserts(entites.stream());
				assertThat(test1.getId(), is(idVal));
				assertThat(test2.getId(), is(idVal + 1));
				assertThat(test3.getId(), is(idVal + 2));

				assertThat(agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatch() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestEntityWithId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatchNoId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				List<TestAutoEntityWithNoId> entites = new ArrayList<>();
				TestAutoEntityWithNoId test1 = new TestAutoEntityWithNoId("name1");
				entites.add(test1);

				TestAutoEntityWithNoId test2 = new TestAutoEntityWithNoId("name2");
				entites.add(test2);

				TestAutoEntityWithNoId test3 = new TestAutoEntityWithNoId("name3");
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestAutoEntityWithNoId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatchNoIdNotSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				List<TestAutoEntityWithNoIdObj> entites = new ArrayList<>();
				TestAutoEntityWithNoIdObj test1 = new TestAutoEntityWithNoIdObj("name1");
				entites.add(test1);

				TestAutoEntityWithNoIdObj test2 = new TestAutoEntityWithNoIdObj("name2");
				entites.add(test2);

				TestAutoEntityWithNoIdObj test3 = new TestAutoEntityWithNoIdObj("name3");
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatchNoIdSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = agent.queryWith(
						"select sequence_schema, sequence_name, current_value, increment from information_schema.sequences order by sequence_name")
						.stream()
						.filter(map -> Objects.toString(map.get("SEQUENCE_NAME")).startsWith("SYSTEM_SEQUENCE"))
						.mapToLong(map -> (Long) map.get("CURRENT_VALUE"))
						.findFirst().getAsLong();

				long idVal = currVal + 100;

				List<TestAutoEntityWithNoIdObj> entites = new ArrayList<>();
				TestAutoEntityWithNoIdObj test1 = new TestAutoEntityWithNoIdObj("name1");
				test1.id = idVal + 1;
				entites.add(test1);

				TestAutoEntityWithNoIdObj test2 = new TestAutoEntityWithNoIdObj("name2");
				test2.id = idVal + 2;
				entites.add(test2);

				TestAutoEntityWithNoIdObj test3 = new TestAutoEntityWithNoIdObj("name3");
				test3.id = idVal + 3;
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test3.getId(), is(idVal + 3));

				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestAutoEntityWithNoIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatchWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				test1.id = idVal;
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				test2.id = idVal + 1;
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				test3.id = idVal + 2;
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));

				assertThat(agent.find(TestEntityWithId.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithId.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithId.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsBatchWithObjectKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				List<TestEntityWithIdObj> entites = new ArrayList<>();
				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				test1.id = idVal;
				entites.add(test1);

				TestEntityWithIdObj test2 = new TestEntityWithIdObj("name2");
				test2.id = idVal + 1;
				entites.add(test2);

				TestEntityWithIdObj test3 = new TestEntityWithIdObj("name3");
				test3.id = idVal + 2;
				entites.add(test3);

				agent.inserts(entites.stream(), InsertsType.BATCH);
				assertThat(test1.getId(), is(idVal));
				assertThat(test2.getId(), is(idVal + 1));
				assertThat(test3.getId(), is(idVal + 2));

				assertThat(agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null), is(test1));
				assertThat(agent.find(TestEntityWithIdObj.class, test2.getId()).orElse(null), is(test2));
				assertThat(agent.find(TestEntityWithIdObj.class, test3.getId()).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testInsertsAndReturn() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				entites.add(test3);

				List<TestEntityWithId> insertedEntities = agent.insertsAndReturn(entites.stream())
						.collect(Collectors.toList());
				assertThat(insertedEntities.get(0).getId(), is(++currVal));
				assertThat(insertedEntities.get(1).getId(), is(++currVal));
				assertThat(insertedEntities.get(2).getId(), is(++currVal));

				assertThat(insertedEntities.get(0), is(test1));
				assertThat(insertedEntities.get(1), is(test2));
				assertThat(insertedEntities.get(2), is(test3));
			});
		}
	}

	@Test
	public void testInsertsAndReturnBatch() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				List<TestEntityWithId> entites = new ArrayList<>();
				TestEntityWithId test1 = new TestEntityWithId("name1");
				entites.add(test1);

				TestEntityWithId test2 = new TestEntityWithId("name2");
				entites.add(test2);

				TestEntityWithId test3 = new TestEntityWithId("name3");
				entites.add(test3);

				List<TestEntityWithId> insertedEntities = agent.insertsAndReturn(entites.stream(), InsertsType.BATCH)
						.collect(Collectors.toList());
				assertThat(insertedEntities.get(0).getId(), is(++currVal));
				assertThat(insertedEntities.get(1).getId(), is(++currVal));
				assertThat(insertedEntities.get(2).getId(), is(++currVal));

				assertThat(insertedEntities.get(0), is(test1));
				assertThat(insertedEntities.get(1), is(test2));
				assertThat(insertedEntities.get(2), is(test3));
			});
		}
	}

	@Test
	public void testInsertAndUpdate() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithId test1 = new TestEntityWithId("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data.getName(), is("name1"));

				test1.setName("rename1");
				agent.update(test1);

				data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data.getName(), is("rename1"));

			});
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testEntityNotGeneratedValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithIdError test1 = new TestEntityWithIdError("name1");
				agent.insert(test1);
			});
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testEntityInvalidTypeGeneratedValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithIdError2 test1 = new TestEntityWithIdError2("name1");
				agent.insert(test1);
			});
		}
	}

	@Test
	public void testInsertMultiKey() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertMultiKeyWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				// 複数のIDすべてに値を設定した場合
				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				test1.id = idCurrVal + 10;
				test1.id2 = id2CurrVal + 10;

				agent.insert(test1);
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));

				// 複数のIDの片方に値を設定した場合
				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				test2.id = idCurrVal + 10;

				agent.insert(test2);
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				test3.id2 = id2CurrVal + 10;

				agent.insert(test3);
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertMultiKeyWithObjectKeyValueNotSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));

				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));

				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertMultiKeyWithObjectKeyValueSetId() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				long idVal = idCurrVal + 100;
				long id2Val = id2CurrVal + 100;

				// 複数のIDすべてに値を設定した場合
				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				test1.id = idVal;
				test1.id2 = id2Val;

				agent.insert(test1);
				assertThat(test1.getId(), is(idVal));
				assertThat(test1.getId2(), is(id2Val));

				// 複数のIDの片方に値を設定した場合
				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				test2.id = ++idVal;

				agent.insert(test2);
				assertThat(test2.getId(), is(idVal));
				assertThat(test2.getId2(), is(++id2CurrVal));

				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				test3.id2 = ++id2Val;

				agent.insert(test3);
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(id2Val));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithId test1 = new TestEntityWithId("name1");
				TestEntityWithId test2 = new TestEntityWithId("name2");
				TestEntityWithId test3 = new TestEntityWithId("name3");
				TestEntityWithId test4 = new TestEntityWithId("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithId.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInsertWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				TestEntityWithId test1 = new TestEntityWithId("name1");
				test1.id = idVal + 1;
				TestEntityWithId test2 = new TestEntityWithId("name2");
				test2.id = idVal + 2;
				TestEntityWithId test3 = new TestEntityWithId("name3");
				test3.id = idVal + 3;
				TestEntityWithId test4 = new TestEntityWithId("name4");
				test4.id = idVal + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithId.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInsertWithObjectKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				test1.id = idVal + 1;
				TestEntityWithIdObj test2 = new TestEntityWithIdObj("name2");
				test2.id = idVal + 2;
				TestEntityWithIdObj test3 = new TestEntityWithIdObj("name3");
				test3.id = idVal + 3;
				TestEntityWithIdObj test4 = new TestEntityWithIdObj("name4");
				test4.id = idVal + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test3.getId(), is(idVal + 3));
				assertThat(test4.getId(), is(idVal + 4));

				TestEntityWithIdObj data = agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithIdObj.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithIdObj.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithIdObj.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInserMultikey() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				TestEntityWithMultiId test4 = new TestEntityWithMultiId("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiId.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInserMultikeyWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long idVal = idCurrVal + 100;
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				test1.id = idVal + 1;
				test1.id2 = id2Val + 1;
				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				test1.id = idVal + 2;
				test1.id2 = id2Val + 2;
				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				test1.id = idVal + 3;
				test1.id2 = id2Val + 3;
				TestEntityWithMultiId test4 = new TestEntityWithMultiId("name4");
				test1.id = idVal + 4;
				test1.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiId.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInserMultikeyWithObjectKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long idVal = idCurrVal + 100;
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				test1.id = idVal + 1;
				test1.id2 = id2Val + 1;
				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				test2.id = idVal + 2;
				test2.id2 = id2Val + 2;
				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				test3.id = idVal + 3;
				test3.id2 = id2Val + 3;
				TestEntityWithMultiIdObj test4 = new TestEntityWithMultiIdObj("name4");
				test4.id = idVal + 4;
				test4.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test1.getId2(), is(id2Val + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test2.getId2(), is(id2Val + 2));
				assertThat(test3.getId(), is(idVal + 3));
				assertThat(test3.getId2(), is(id2Val + 3));
				assertThat(test4.getId(), is(idVal + 4));
				assertThat(test4.getId2(), is(id2Val + 4));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiIdObj.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBulkInserMultikeyWithObjectKeyValue2() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				test1.id2 = id2Val + 1;
				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				test2.id2 = id2Val + 2;
				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				test3.id2 = id2Val + 3;
				TestEntityWithMultiIdObj test4 = new TestEntityWithMultiIdObj("name4");
				test4.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(id2Val + 1));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(id2Val + 2));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(id2Val + 3));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(id2Val + 4));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiIdObj.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsert() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithId test1 = new TestEntityWithId("name1");
				TestEntityWithId test2 = new TestEntityWithId("name2");
				TestEntityWithId test3 = new TestEntityWithId("name3");
				TestEntityWithId test4 = new TestEntityWithId("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithId.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				TestEntityWithId test1 = new TestEntityWithId("name1");
				test1.id = idVal + 1;
				TestEntityWithId test2 = new TestEntityWithId("name2");
				test2.id = idVal + 2;
				TestEntityWithId test3 = new TestEntityWithId("name3");
				test3.id = idVal + 3;
				TestEntityWithId test4 = new TestEntityWithId("name4");
				test4.id = idVal + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithId data = agent.find(TestEntityWithId.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithId.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithId.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithId.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertWithObjectKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");
				long idVal = currVal + 100;

				TestEntityWithIdObj test1 = new TestEntityWithIdObj("name1");
				test1.id = idVal + 1;
				TestEntityWithIdObj test2 = new TestEntityWithIdObj("name2");
				test2.id = idVal + 2;
				TestEntityWithIdObj test3 = new TestEntityWithIdObj("name3");
				test3.id = idVal + 3;
				TestEntityWithIdObj test4 = new TestEntityWithIdObj("name4");
				test4.id = idVal + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test3.getId(), is(idVal + 3));
				assertThat(test4.getId(), is(idVal + 4));

				TestEntityWithIdObj data = agent.find(TestEntityWithIdObj.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithIdObj.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithIdObj.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithIdObj.class, test4.getId()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertMultikey() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				TestEntityWithMultiId test4 = new TestEntityWithMultiId("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiId.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertMultikeyWithPrimitiveKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long idVal = idCurrVal + 100;
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiId test1 = new TestEntityWithMultiId("name1");
				test1.id = idVal + 1;
				test1.id2 = id2Val + 1;
				TestEntityWithMultiId test2 = new TestEntityWithMultiId("name2");
				test2.id = idVal + 2;
				test2.id2 = id2Val + 2;
				TestEntityWithMultiId test3 = new TestEntityWithMultiId("name3");
				test3.id = idVal + 3;
				test3.id2 = id2Val + 3;
				TestEntityWithMultiId test4 = new TestEntityWithMultiId("name4");
				test4.id = idVal + 4;
				test4.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(++id2CurrVal));

				TestEntityWithMultiId data = agent.find(TestEntityWithMultiId.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiId.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiId.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiId.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertMultikeyWithObjectKeyValue() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long idVal = idCurrVal + 100;
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				test1.id = idVal + 1;
				test1.id2 = id2Val + 1;
				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				test2.id = idVal + 2;
				test2.id2 = id2Val + 2;
				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				test3.id = idVal + 3;
				test3.id2 = id2Val + 3;
				TestEntityWithMultiIdObj test4 = new TestEntityWithMultiIdObj("name4");
				test4.id = idVal + 4;
				test4.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(idVal + 1));
				assertThat(test1.getId2(), is(id2Val + 1));
				assertThat(test2.getId(), is(idVal + 2));
				assertThat(test2.getId2(), is(id2Val + 2));
				assertThat(test3.getId(), is(idVal + 3));
				assertThat(test3.getId2(), is(id2Val + 3));
				assertThat(test4.getId(), is(idVal + 4));
				assertThat(test4.getId2(), is(id2Val + 4));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiIdObj.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Test
	public void testBatchInsertMultikeyWithObjectKeyValue2() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");
				long id2Val = id2CurrVal + 100;

				TestEntityWithMultiIdObj test1 = new TestEntityWithMultiIdObj("name1");
				test1.id2 = id2Val + 1;
				TestEntityWithMultiIdObj test2 = new TestEntityWithMultiIdObj("name2");
				test2.id2 = id2Val + 2;
				TestEntityWithMultiIdObj test3 = new TestEntityWithMultiIdObj("name3");
				test3.id2 = id2Val + 3;
				TestEntityWithMultiIdObj test4 = new TestEntityWithMultiIdObj("name4");
				test4.id2 = id2Val + 4;

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(id2Val + 1));
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(id2Val + 2));
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(id2Val + 3));
				assertThat(test4.getId(), is(++idCurrVal));
				assertThat(test4.getId2(), is(id2Val + 4));

				TestEntityWithMultiIdObj data = agent
						.find(TestEntityWithMultiIdObj.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithMultiIdObj.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithMultiIdObj.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithMultiIdObj.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Table(name = "TEST")
	public static class TestEntityWithId {
		@Id
		@GeneratedValue
		private long id;
		private String name;

		public TestEntityWithId() {
		}

		public TestEntityWithId(final String name) {
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithId other = (TestEntityWithId) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithId [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST_AUTO")
	public static class TestAutoEntityWithNoId {
		private long id;
		private String name;

		public TestAutoEntityWithNoId() {
		}

		public TestAutoEntityWithNoId(final String name) {
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestAutoEntityWithNoId other = (TestAutoEntityWithNoId) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestAutoEntityWithNoId [id=" + id + ", name=" + name + "]";
		}
	}

	@Table(name = "TEST_AUTO")
	public static class TestAutoEntityWithNoIdObj {
		private Long id;
		private String name;

		public TestAutoEntityWithNoIdObj() {
		}

		public TestAutoEntityWithNoIdObj(final String name) {
			this.name = name;
		}

		public Long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final Long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (id == null ? 0 : id.hashCode());
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestAutoEntityWithNoIdObj other = (TestAutoEntityWithNoIdObj) obj;
			if (id == null) {
				if (other.id != null) {
					return false;
				}
			} else if (!id.equals(other.id)) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestAutoEntityWithNoIdObj [id=" + id + ", name=" + name + "]";
		}
	}

	@Table(name = "TEST")
	public static class TestEntityWithIdObj {
		@Id
		@GeneratedValue
		private Long id;
		private String name;

		public TestEntityWithIdObj() {
		}

		public TestEntityWithIdObj(final String name) {
			this.name = name;
		}

		public Long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final Long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (id == null ? 0 : id.hashCode());
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithIdObj other = (TestEntityWithIdObj) obj;
			if (id == null) {
				if (other.id != null) {
					return false;
				}
			} else if (!id.equals(other.id)) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithIdObj [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST")
	public static class TestEntityWithIdError {
		@Id
		private long id;
		private String name;

		public TestEntityWithIdError() {
		}

		public TestEntityWithIdError(final String name) {
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithIdError other = (TestEntityWithIdError) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithIdError [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST")
	public static class TestEntityWithIdError2 {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private double id;
		private String name;

		public TestEntityWithIdError2() {
		}

		public TestEntityWithIdError2(final String name) {
			this.name = name;
		}

		public double getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final double id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			long temp;
			temp = Double.doubleToLongBits(id);
			result = prime * result + (int) (temp ^ temp >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithIdError2 other = (TestEntityWithIdError2) obj;
			if (Double.doubleToLongBits(id) != Double.doubleToLongBits(other.id)) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithIdError2 [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST_MULTIKEY")
	public static class TestEntityWithMultiId {
		@Id
		@GeneratedValue
		private long id;
		@Id
		@GeneratedValue
		private long id2;

		private String name;

		public TestEntityWithMultiId() {
		}

		public TestEntityWithMultiId(final String name) {
			this.name = name;
		}

		public long getId() {
			return id;
		}

		public long getId2() {
			return id2;
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (int) (id2 ^ id2 >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithMultiId other = (TestEntityWithMultiId) obj;
			if (id != other.id) {
				return false;
			}
			if (id2 != other.id2) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithMultiId [id=" + id + ", id2=" + id2 + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST_MULTIKEY")
	public static class TestEntityWithMultiIdObj {
		@Id
		@GeneratedValue
		private Long id;
		@Id
		@GeneratedValue
		private Long id2;

		private String name;

		public TestEntityWithMultiIdObj() {
		}

		public TestEntityWithMultiIdObj(final String name) {
			this.name = name;
		}

		public long getId() {
			return id;
		}

		public long getId2() {
			return id2;
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (id == null ? 0 : id.hashCode());
			result = prime * result + (id2 == null ? 0 : id2.hashCode());
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntityWithMultiIdObj other = (TestEntityWithMultiIdObj) obj;
			if (id == null) {
				if (other.id != null) {
					return false;
				}
			} else if (!id.equals(other.id)) {
				return false;
			}
			if (id2 == null) {
				if (other.id2 != null) {
					return false;
				}
			} else if (!id2.equals(other.id2)) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntityWithMultiIdObj [id=" + id + ", id2=" + id2 + ", name=" + name + "]";
		}

	}

}
