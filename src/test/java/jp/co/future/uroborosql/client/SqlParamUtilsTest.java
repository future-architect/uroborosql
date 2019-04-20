package jp.co.future.uroborosql.client;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
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
		assertThat((Date) ctx.getParam("key1").getValue(), is(instanceOf(Date.class)));
		assertThat(ctx.hasParam("key2"), is(true));
		assertThat((Time) ctx.getParam("key2").getValue(), is(instanceOf(Time.class)));

		ctx = factory.createSqlContext();
		ctx.setSql(sql);
		SqlParamUtils.getSqlParams(ctx.getSql());
		SqlParamUtils.setSqlParams(ctx, "key1=2019-01-01T10:15:20");
		assertThat(ctx.hasParam("key1"), is(true));
		assertThat((Timestamp) ctx.getParam("key1").getValue(), is(instanceOf(Timestamp.class)));

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

}
