package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
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
import jp.co.future.uroborosql.mapping.annotations.SequenceGenerator;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class SequenceGeneratedKeysTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:SequenceGeneratedKeysTest;DB_CLOSE_DELAY=-1";
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

				TestEntityWithSeq test1 = new TestEntityWithSeq("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestEntityWithSeq test2 = new TestEntityWithSeq("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++currVal));

				TestEntityWithSeq test3 = new TestEntityWithSeq("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++currVal));

				TestEntityWithSeq data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeq.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeq.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test
	public void testInsertAndUpdate() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithSeq test1 = new TestEntityWithSeq("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++currVal));

				TestEntityWithSeq data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data.getName(), is("name1"));

				test1.setName("rename1");
				agent.update(test1);

				data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data.getName(), is("rename1"));

			});
		}
	}

	@Test
	public void testInsertMultikey() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long idCurrVal = (Long) agent.queryWith("select currval('test_multikey_id_seq') as id").findFirst()
						.get().get("ID");
				long id2CurrVal = (Long) agent.queryWith("select currval('test_multikey_id2_seq') as id").findFirst()
						.get().get("ID");

				TestEntityWithSeqMultikey test1 = new TestEntityWithSeqMultikey("name1");
				agent.insert(test1);
				assertThat(test1.getId(), is(++idCurrVal));
				assertThat(test1.getId2(), is(++id2CurrVal));

				TestEntityWithSeqMultikey test2 = new TestEntityWithSeqMultikey("name2");
				agent.insert(test2);
				assertThat(test2.getId(), is(++idCurrVal));
				assertThat(test2.getId2(), is(++id2CurrVal));

				TestEntityWithSeqMultikey test3 = new TestEntityWithSeqMultikey("name3");
				agent.insert(test3);
				assertThat(test3.getId(), is(++idCurrVal));
				assertThat(test3.getId2(), is(++id2CurrVal));

				TestEntityWithSeqMultikey data = agent
						.find(TestEntityWithSeqMultikey.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeqMultikey.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeqMultikey.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testEntityNotSequenceGenerator() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithSeqError test1 = new TestEntityWithSeqError("name1");
				agent.insert(test1);
			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				long currVal = (Long) agent.queryWith("select currval('test_id_seq') as id").findFirst().get()
						.get("ID");

				TestEntityWithSeq test1 = new TestEntityWithSeq("name1");
				TestEntityWithSeq test2 = new TestEntityWithSeq("name2");
				TestEntityWithSeq test3 = new TestEntityWithSeq("name3");
				TestEntityWithSeq test4 = new TestEntityWithSeq("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BULK);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithSeq data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeq.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeq.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithSeq.class, test4.getId()).orElse(null);
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

				TestEntityWithSeqMultikey test1 = new TestEntityWithSeqMultikey("name1");
				TestEntityWithSeqMultikey test2 = new TestEntityWithSeqMultikey("name2");
				TestEntityWithSeqMultikey test3 = new TestEntityWithSeqMultikey("name3");
				TestEntityWithSeqMultikey test4 = new TestEntityWithSeqMultikey("name4");

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

				TestEntityWithSeqMultikey data = agent
						.find(TestEntityWithSeqMultikey.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeqMultikey.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeqMultikey.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithSeqMultikey.class, test4.getId(), test4.getId2()).orElse(null);
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

				TestEntityWithSeq test1 = new TestEntityWithSeq("name1");
				TestEntityWithSeq test2 = new TestEntityWithSeq("name2");
				TestEntityWithSeq test3 = new TestEntityWithSeq("name3");
				TestEntityWithSeq test4 = new TestEntityWithSeq("name4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, idx, entity) -> idx == 2,
						InsertsType.BATCH);
				assertThat(count, is(4));
				assertThat(test1.getId(), is(++currVal));
				assertThat(test2.getId(), is(++currVal));
				assertThat(test3.getId(), is(++currVal));
				assertThat(test4.getId(), is(++currVal));

				TestEntityWithSeq data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeq.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeq.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithSeq.class, test4.getId()).orElse(null);
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

				TestEntityWithSeqMultikey test1 = new TestEntityWithSeqMultikey("name1");
				TestEntityWithSeqMultikey test2 = new TestEntityWithSeqMultikey("name2");
				TestEntityWithSeqMultikey test3 = new TestEntityWithSeqMultikey("name3");
				TestEntityWithSeqMultikey test4 = new TestEntityWithSeqMultikey("name4");

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

				TestEntityWithSeqMultikey data = agent
						.find(TestEntityWithSeqMultikey.class, test1.getId(), test1.getId2())
						.orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeqMultikey.class, test2.getId(), test2.getId2()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeqMultikey.class, test3.getId(), test3.getId2()).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityWithSeqMultikey.class, test4.getId(), test4.getId2()).orElse(null);
				assertThat(data, is(test4));
			});
		}
	}

	@Table(name = "TEST")
	public static class TestEntityWithSeq {
		@Id
		@GeneratedValue(strategy = GenerationType.SEQUENCE)
		@SequenceGenerator(sequence = "test_id_seq")
		private long id;
		private String name;

		public TestEntityWithSeq() {
		}

		public TestEntityWithSeq(final String name) {
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
			TestEntityWithSeq other = (TestEntityWithSeq) obj;
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
			return "TestEntityWithSeq [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST_MULTIKEY")
	public static class TestEntityWithSeqMultikey {
		@Id
		@GeneratedValue(strategy = GenerationType.SEQUENCE)
		@SequenceGenerator(sequence = "test_multikey_id_seq")
		private long id;
		@Id
		@GeneratedValue(strategy = GenerationType.SEQUENCE)
		@SequenceGenerator(sequence = "test_multikey_id2_seq")
		private long id2;
		private String name;

		public TestEntityWithSeqMultikey() {
		}

		public TestEntityWithSeqMultikey(final String name) {
			this.name = name;
		}

		public long getId() {
			return id;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public long getId2() {
			return id2;
		}

		public void setId2(final long id2) {
			this.id2 = id2;
		}

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
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
			TestEntityWithSeqMultikey other = (TestEntityWithSeqMultikey) obj;
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
			return "TestEntityWithSeqMultikey [id=" + id + ", id2=" + id2 + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST")
	public static class TestEntityWithSeqError {
		@Id
		@GeneratedValue(strategy = GenerationType.SEQUENCE)
		private long id;
		private String name;

		public TestEntityWithSeqError() {
		}

		public TestEntityWithSeqError(final String name) {
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
			TestEntityWithSeqError other = (TestEntityWithSeqError) obj;
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
			return "TestEntityWithSeqError [id=" + id + ", name=" + name + "]";
		}

	}

}
