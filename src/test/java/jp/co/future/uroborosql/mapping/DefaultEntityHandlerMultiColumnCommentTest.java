package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls;

/**
 * カラムコメント（remark）に改行が含まれる場合のテストケース
 *
 * @author H.Sugimoto
 */
public class DefaultEntityHandlerMultiColumnCommentTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:" + DefaultEntityHandlerMultiColumnCommentTest.class.getSimpleName()
				+ ";DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");
				stmt.execute(
						"create table if not exists test( \"ID\" NUMERIC(4),name VARCHAR(10),\"Age\" NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version INTEGER, primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.\"ID\" is 'id'");
				stmt.execute("comment on column test.name is 'name\r\n" + " comment.'");
				stmt.execute("comment on column test.\"Age\" is 'age\r" + " comment.'");
				stmt.execute("comment on column test.birthday is 'birthday\n" + " comment.'");
				stmt.execute("comment on column test.memo is 'memo'");
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
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2), Optional.empty());
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3), Optional
						.of("memo3"));
				agent.insert(test3);
				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testQuery1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1), Optional
						.of("memo2"));
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 22, LocalDate.of(1990, Month.MAY, 1), Optional.empty());
				agent.insert(test3);

				List<TestEntity> list = agent.query(TestEntity.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class)
						.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	public void testQueryWithCondition() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 22, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1),
						Optional.of("memo2"));
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 20, LocalDate.of(1990, Month.JUNE, 1), Optional.empty());
				agent.insert(test3);

				// Equal
				List<TestEntity> list = null;
				list = agent.query(TestEntity.class).equal("id", 2).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// Not Equal
				list = agent.query(TestEntity.class).notEqual("id", 2).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				// Greater Than
				list = agent.query(TestEntity.class).greaterThan("age", 21).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// Greater Equal
				list = agent.query(TestEntity.class).greaterEqual("age", 21).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// Less Than
				list = agent.query(TestEntity.class).lessThan("age", 21).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Greater Equal
				list = agent.query(TestEntity.class).lessEqual("age", 21).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// In (array)
				list = agent.query(TestEntity.class).in("id", 1, 2).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// In (list)
				list = agent.query(TestEntity.class).in("id", Arrays.asList(2, 3)).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// Not In (array)
				list = agent.query(TestEntity.class).notIn("id", 1, 2).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Not In (list)
				list = agent.query(TestEntity.class).notIn("id", Arrays.asList(2, 3)).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// Like
				list = agent.query(TestEntity.class).like("name", "name3").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Like with wildcards (_)
				list = agent.query(TestEntity.class).like("name", "n_me_").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// Like with wildcards (%)
				list = agent.query(TestEntity.class).like("name", "name%").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// startsWith
				list = agent.query(TestEntity.class).startsWith("name", "name").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// startsWith wildcards
				list = agent.query(TestEntity.class).startsWith("name", "%ame").collect();
				assertThat(list.size(), is(0));

				// endsWith
				list = agent.query(TestEntity.class).endsWith("name", "3").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// endsWith wildcards
				list = agent.query(TestEntity.class).endsWith("name", "%3").collect();
				assertThat(list.size(), is(0));

				// contains
				list = agent.query(TestEntity.class).contains("name", "me").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class).contains("name", "%me_").collect();
				assertThat(list.size(), is(0));

				// Not Like
				list = agent.query(TestEntity.class).notLike("name", "name3").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// Not Like with wildcards (_)
				list = agent.query(TestEntity.class).notLike("name", "name_").collect();
				assertThat(list.size(), is(0));

				// Not Like with wildcards (%)
				list = agent.query(TestEntity.class).notLike("name", "name%").collect();
				assertThat(list.size(), is(0));

				// notStartsWith
				list = agent.query(TestEntity.class).notStartsWith("name", "name").collect();
				assertThat(list.size(), is(0));

				// notStartsWith wildcards
				list = agent.query(TestEntity.class).notStartsWith("name", "%name").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notEndsWith
				list = agent.query(TestEntity.class).notEndsWith("name", "3").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// notEndsWith wildcards
				list = agent.query(TestEntity.class).notEndsWith("name", "%3").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notContains
				list = agent.query(TestEntity.class).notContains("name", "2").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				// notContains wildcards
				list = agent.query(TestEntity.class).notContains("name", "_2").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// Between
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 15), LocalDate.of(1990, Month.MAY, 15))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 1), LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// is null
				list = agent.query(TestEntity.class).isNull("memo").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// is not null
				list = agent.query(TestEntity.class).isNotNull("memo").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// where with param(s)
				Map<String, Object> paramMap = new HashMap<>();
				paramMap.put("birthday1", LocalDate.of(1990, Month.APRIL, 15));
				paramMap.put("birthday2", LocalDate.of(1990, Month.MAY, 15));
				list = agent.query(TestEntity.class)
						.where("birthday < /*birthday1*/ or BIRTHDAY > /*birthday2*/", paramMap)
						.where("\"Age\" < /*age*/", "age", 21)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// where
				list = agent.query(TestEntity.class)
						.where("birthday < /*birthday1*/ or BIRTHDAY > /*birthday2*/", paramMap)
						.where("\"Age\" < 21")
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// order by asc
				list = agent.query(TestEntity.class)
						.asc("age")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by asc
				list = agent.query(TestEntity.class)
						.asc("age", Nulls.FIRST)
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by desc
				list = agent.query(TestEntity.class)
						.desc("birthday")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by asc and desc
				list = agent.query(TestEntity.class)
						.asc("age")
						.desc("birthday", Nulls.FIRST)
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// limit, offset
				list = agent.query(TestEntity.class)
						.limit(2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class)
						.limit(2)
						.offset(1)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// count
				long count1 = agent.query(TestEntity.class).asc("age").count();
				assertThat(count1, is(3L));

				long count2 = agent.query(TestEntity.class).lessEqual("age", 21).count();
				assertThat(count2, is(2L));

				long count3 = agent.query(TestEntity.class).lessEqual("age", 21).count("memo");
				assertThat(count3, is(1L));

			});
		}
	}

	@Test
	public void testUpdate1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testDelete1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testCreateSelectContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createSelectContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			try (ResultSet rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}
		}
	}

	@Test
	public void testCreateInsertContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createInsertContext(agent, metadata, null);

			String sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			ctx.param("id", 1).param("name", "name1").param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1)).param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));
		}
	}

	@Test
	public void testCreateInsertContextEmptyNotEqualsNull() throws Exception {
		try (SqlAgent agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createInsertContext(agent, metadata, null);

			String sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			ctx.param("id", 1).param("name", "name1").param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1)).param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	public void testCreateUpdateContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createUpdateContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, containsString("IF memo != null"));

			ctx.param("id", 1).param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).equal("id", 1).first().orElse(null).getName(), is("updatename"));
		}
	}

	@Test
	public void testCreateDeleteContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createDeleteContext(agent, metadata, null, true);
			ctx.param("id", 1);
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).equal("id", 1).first().orElse(null), is(nullValue()));
		}
	}

}
