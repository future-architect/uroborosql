package jp.co.future.uroborosql.client;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;

public class SqlParamUtilsTest {

	@SuppressWarnings("unchecked")
	@Test
	public void testSetSqlParams() {
		SqlContextFactory factory = new SqlContextFactoryImpl();
		factory.initialize();
		String sql = "/*key1*/, /*key2*/, /*key3*/(), /*CLS_AGE_DEFALUT*/, /*$CLS_FLAG_OFF*/";
		SqlContext ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=1", "key2", "key3=[true, false]");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1));
		assertThat(ctx.hasParam("key2"), is(true));
		assertThat(ctx.getParam("key2").getValue(), is(nullValue()));
		assertThat(ctx.hasParam("key3"), is(true));
		assertThat((List<Boolean>) ctx.getParam("key3").getValue(), hasItem(true));
		assertThat((List<Boolean>) ctx.getParam("key3").getValue(), hasItem(false));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=[NULL]", "key2=[EMPTY]");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(nullValue()));
		assertThat(ctx.hasParam("key2"), is(true));
		assertThat(ctx.getParam("key2").getValue(), is(""));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key3=[10, 20, 30]", "key1='multi word'");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is("multi word"));
		assertThat(ctx.hasParam("key2"), is(true));
		assertThat(ctx.getParam("key2").getValue(), is(nullValue()));
		assertThat(ctx.hasParam("key3"), is(true));
		List<Integer> val3 = (List<Integer>) ctx.getParam("key3").getValue();
		assertThat(val3, hasItem(10));
		assertThat(val3, hasItem(20));
		assertThat(val3, hasItem(30));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=2019-01-01", "key2=10:15:20");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(instanceOf(LocalDate.class)));
		assertThat(ctx.hasParam("key2"), is(true));
		assertThat(ctx.getParam("key2").getValue(), is(instanceOf(LocalTime.class)));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=2019-01-01T10:15:20");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(instanceOf(LocalDateTime.class)));

		// int
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=1000");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=+1000");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=-1000");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(-1000));

		// 桁あふれ
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=" + Integer.MAX_VALUE + "0");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(new BigDecimal(Integer.MAX_VALUE + "0")));

		// long
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=1000L");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000L));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=+1000L");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000L));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=-1000L");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(-1000L));

		// 桁あふれ
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=" + Long.MAX_VALUE + "0");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(new BigDecimal(Long.MAX_VALUE + "0")));

		// float
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=1000.01F");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000.01F));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=+1000.01f");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000.01F));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=-1000.01F");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(-1000.01F));

		// double
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=1000.01D");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000.01D));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=+1000.01d");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(1000.01D));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=-1000.01D");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(-1000.01D));

		// 数字の集合
		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=0000010");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is("0000010"));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=  ");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat(ctx.getParam("key1").getValue(), is(nullValue()));
	}

	@Test
	public void testSetSqlParamsIfNode() {
		SqlContextFactory factory = new SqlContextFactoryImpl();
		factory.initialize();
		String sql = "/*IF check1 != null*/ /*key1*/ /*ELSE*/ /*key2*/ /*END*/";
		SqlContext ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=value1", "key2=value2");
	}

	@Test
	public void testGetSqlParams() {
		Set<String> params = SqlParamUtils
				.getSqlParams(
						"select * from test where id = /*id*/1 and name = /*name*/'name1 age = /*CLS_AGE_DEFALUT*/0");
		assertThat(params, contains("id", "name"));
		assertThat(params, not(contains("CLS_AGE_DEFAULT")));
	}

	@Test
	public void testParseLine() {
		String[] parts = SqlParamUtils.parseLine("update /sss/bbb param1=[1, 2, 3] param2=3 param3=[1, 2]");
		assertThat(parts, arrayContaining("update", "/sss/bbb", "param1=[1,2,3]", "param2=3", "param3=[1,2]"));

		parts = SqlParamUtils.parseLine("update /sss/bbb param1=[NULL] param2=[EMPTY]");
		assertThat(parts, arrayContaining("update", "/sss/bbb", "param1=[NULL]", "param2=[EMPTY]"));

		parts = SqlParamUtils.parseLine("update /sss/bbb param1=['o n e', 't w o'] param2='s p a c e'");
		assertThat(parts, arrayContaining("update", "/sss/bbb", "param1=['o n e','t w o']", "param2='s p a c e'"));
	}

}
