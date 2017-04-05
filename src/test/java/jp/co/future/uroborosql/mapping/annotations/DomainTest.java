package jp.co.future.uroborosql.mapping.annotations;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.Objects;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

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

		config = DefaultSqlConfig.getConfig(url, user, password);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@Test
	public void test() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@Test
	public void test2() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
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
			return Objects.hashCode(this.name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj instanceof NameDomain3) {
				return Objects.equals(this.name, ((NameDomain3) obj).name);
			}
			return false;
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	public static class NameDomain3Impl extends NameDomain3 {
		NameDomain3Impl(final String name) {
			super(name);
		}
	}

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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@Test
	public void test3() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
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
	public static enum NameDomain4 {
		NAME1, NAME2, NAME3,;
	}

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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@Test
	public void test4() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
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
