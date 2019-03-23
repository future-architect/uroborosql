package jp.co.future.uroborosql.utils;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.dialect.DefaultDialect;
import ognl.Ognl;
import ognl.OgnlContext;

public class StringFunctionTest {
	private StringFunction expressionFunction;

	@Before
	public void setUp() throws Exception {
		expressionFunction = new StringFunction(new DefaultDialect());
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws Exception {
		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val1", null);
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		Ognl.parseExpression("SF.isEmpty(val1)");

		assertTrue((boolean) Ognl.getValue("SF.isEmpty(val1)", context, root, null));
	}

	@Test
	public void testStartsWith() throws Exception {
		assertThat(expressionFunction.startsWith("abc"), is("abc%"));
		assertThat(expressionFunction.startsWith("a_bc"), is("a$_bc%"));
		assertThat(expressionFunction.startsWith("a%bc"), is("a$%bc%"));
		assertThat(expressionFunction.startsWith(""), is("%"));
		assertThat(expressionFunction.startsWith(null), is("%"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.startsWith(val)"), context, root), is("abc%"));
	}

	@Test(expected = IllegalStateException.class)
	public void testStartsWithNoDialect() throws Exception {
		new StringFunction().startsWith("abc");
	}

	@Test
	public void testContains() throws Exception {
		assertThat(expressionFunction.contains("abc"), is("%abc%"));
		assertThat(expressionFunction.contains("a_bc"), is("%a$_bc%"));
		assertThat(expressionFunction.contains("a%bc"), is("%a$%bc%"));
		assertThat(expressionFunction.contains(""), is("%"));
		assertThat(expressionFunction.contains(null), is("%"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.contains(val)"), context, root), is("%abc%"));
	}

	@Test(expected = IllegalStateException.class)
	public void testContainsNoDialect() throws Exception {
		new StringFunction().contains("abc");
	}

	@Test
	public void testEndsWith() throws Exception {
		assertThat(expressionFunction.endsWith("abc"), is("%abc"));
		assertThat(expressionFunction.endsWith("a_bc"), is("%a$_bc"));
		assertThat(expressionFunction.endsWith("a%bc"), is("%a$%bc"));
		assertThat(expressionFunction.endsWith(""), is("%"));
		assertThat(expressionFunction.endsWith(null), is("%"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.endsWith(val)"), context, root), is("%abc"));
	}

	@Test(expected = IllegalStateException.class)
	public void testEndsWithNoDialect() throws Exception {
		new StringFunction().endsWith("abc");
	}

}
