package jp.co.future.uroborosql.mapping.annotations;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

public class DomainTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:DomainTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10), primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password).build();

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Domain(valueType = String.class, toJdbcMethod = "getName")
	public static class NameDomain {
		private final String name;

		public NameDomain(final String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
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
			NameDomain other = (NameDomain) obj;
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
			return "NameDomain [name=" + name + "]";
		}
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class DomainTestEntity {
		private long id;
		private NameDomain name;

		public DomainTestEntity() {
		}

		public DomainTestEntity(final long id, final String name) {
			this.id = id;
			this.name = name != null ? new NameDomain(name) : null;
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
			DomainTestEntity other = (DomainTestEntity) obj;
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
			return "DomainTestEntity [id=" + id + ", name=" + name + "]";
		}
	}

	@Test
	public void test() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				DomainTestEntity test1 = new DomainTestEntity(1, "name1");
				agent.insert(test1);
				DomainTestEntity test2 = new DomainTestEntity(2, "name2");
				agent.insert(test2);
				DomainTestEntity test3 = new DomainTestEntity(3, null);
				agent.insert(test3);
				DomainTestEntity data = agent.find(DomainTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(DomainTestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(DomainTestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Domain(valueType = String.class, factoryMethod = "of", toJdbcMethod = "getName", nullable = true)
	public static class NameDomain2 {
		private final String name;

		private NameDomain2(final String name) {
			this.name = name;
		}

		public static NameDomain2 of(final String name) {
			return new NameDomain2(name);
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
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
			NameDomain2 other = (NameDomain2) obj;
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
			return "NameDomain2 [name=" + name + "]";
		}
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class DomainTestEntity2 {
		private long id;
		private NameDomain2 name;

		public DomainTestEntity2() {
		}

		public DomainTestEntity2(final long id, final String name) {
			this.id = id;
			this.name = new NameDomain2(name) {
				// 継承しても大丈夫
			};
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
			DomainTestEntity2 other = (DomainTestEntity2) obj;
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
			return "DomainTestEntity2 [id=" + id + ", name=" + name + "]";
		}
	}

	@Test
	public void test2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				DomainTestEntity2 test1 = new DomainTestEntity2(1, "name1");
				agent.insert(test1);
				DomainTestEntity2 test2 = new DomainTestEntity2(2, "name2");
				agent.insert(test2);
				DomainTestEntity2 test3 = new DomainTestEntity2(3, null);
				agent.insert(test3);
				DomainTestEntity2 data = agent.find(DomainTestEntity2.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(DomainTestEntity2.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(DomainTestEntity2.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Domain(valueType = String.class, factoryMethod = "of", toJdbcMethod = "getValue", nullable = true)
	public static class NameDomain3 {
		private final String name;

		NameDomain3(final String name) {
			this.name = name;
		}

		public static NameDomain3Impl of(final String name) {
			return new NameDomain3Impl(name);
		}

		public static String getValue(final NameDomain3 nameDomain3) {
			return nameDomain3.name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
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
			NameDomain3 other = (NameDomain3) obj;
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
			return "NameDomain3 [name=" + name + "]";
		}
	}

	public static class NameDomain3Impl extends NameDomain3 {
		NameDomain3Impl(final String name) {
			super(name);
		}
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class DomainTestEntity3 {
		private long id;
		private NameDomain3 name;

		public DomainTestEntity3() {
		}

		public DomainTestEntity3(final long id, final String name) {
			this.id = id;
			this.name = new NameDomain3(name) {
				// 継承しても大丈夫
			};
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
			DomainTestEntity3 other = (DomainTestEntity3) obj;
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
			return "DomainTestEntity3 [id=" + id + ", name=" + name + "]";
		}
	}

	@Test
	public void test3() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				DomainTestEntity3 test1 = new DomainTestEntity3(1, "name1");
				agent.insert(test1);
				DomainTestEntity3 test2 = new DomainTestEntity3(2, "name2");
				agent.insert(test2);
				DomainTestEntity3 test3 = new DomainTestEntity3(3, null);
				agent.insert(test3);
				DomainTestEntity3 data = agent.find(DomainTestEntity3.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(DomainTestEntity3.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(DomainTestEntity3.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Domain(valueType = String.class, toJdbcMethod = "name")
	public enum NameDomain4 {
		NAME1, NAME2, NAME3,;
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class DomainTestEntity4 {
		private long id;
		private NameDomain4 name;

		public DomainTestEntity4() {
		}

		public DomainTestEntity4(final long id, final NameDomain4 name) {
			this.id = id;
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
			DomainTestEntity4 other = (DomainTestEntity4) obj;
			if (id != other.id) {
				return false;
			}
			if (name != other.name) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "DomainTestEntity4 [id=" + id + ", name=" + name + "]";
		}
	}

	@Test
	public void test4() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				DomainTestEntity4 test1 = new DomainTestEntity4(1, NameDomain4.NAME1);
				agent.insert(test1);
				DomainTestEntity4 test2 = new DomainTestEntity4(2, NameDomain4.NAME2);
				agent.insert(test2);
				DomainTestEntity4 test3 = new DomainTestEntity4(3, null);
				agent.insert(test3);
				DomainTestEntity4 data = agent.find(DomainTestEntity4.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(DomainTestEntity4.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(DomainTestEntity4.class, 3).orElse(null);
				assertThat(data, is(test3));

				DomainTestEntity plainData = agent.find(DomainTestEntity.class, 1).orElse(null);
				assertThat(plainData.name.getName(), is("NAME1"));
				System.out.println(plainData.name.getName());

			});
		}
	}
}
