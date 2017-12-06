package jp.co.future.uroborosql.utils;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import ognl.Ognl;
import ognl.OgnlContext;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class StringFunctionTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws Exception {
		Map<Object, Object> root = new HashMap<Object, Object>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val1", null);
		root.put(StringFunction.SHORT_NAME, new StringFunction());

		Ognl.parseExpression("SF.isEmpty(val1)");

		assertTrue((boolean) Ognl.getValue("SF.isEmpty(val1)", context, root, null));
	}
}
