package jp.co.future.uroborosql.utils;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.hamcrest.Matchers;
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

	@Test
	public void testIsBlank() throws Exception {
		assertThat(expressionFunction.isBlank(null), is(true));
		assertThat(expressionFunction.isBlank(""), is(true));
		assertThat(expressionFunction.isBlank(" "), is(true));
		assertThat(expressionFunction.isBlank("bob"), is(false));
		assertThat(expressionFunction.isBlank("  bob  "), is(false));
		assertThat(expressionFunction.isBlank(123), is(false));
	}

	@Test
	public void testIsEmpty() throws Exception {
		assertThat(expressionFunction.isEmpty(null), is(true));
		assertThat(expressionFunction.isEmpty(""), is(true));
		assertThat(expressionFunction.isEmpty(" "), is(false));
		assertThat(expressionFunction.isEmpty("bob"), is(false));
		assertThat(expressionFunction.isEmpty("  bob  "), is(false));
		assertThat(expressionFunction.isEmpty(123), is(false));
	}

	@Test
	public void testIsNotBlank() throws Exception {
		assertThat(expressionFunction.isNotBlank(null), is(false));
		assertThat(expressionFunction.isNotBlank(""), is(false));
		assertThat(expressionFunction.isNotBlank(" "), is(false));
		assertThat(expressionFunction.isNotBlank("bob"), is(true));
		assertThat(expressionFunction.isNotBlank("  bob  "), is(true));
		assertThat(expressionFunction.isNotBlank(123), is(true));
	}

	@Test
	public void testIsNotEmpty() throws Exception {
		assertThat(expressionFunction.isNotEmpty(null), is(false));
		assertThat(expressionFunction.isNotEmpty(""), is(false));
		assertThat(expressionFunction.isNotEmpty(" "), is(true));
		assertThat(expressionFunction.isNotEmpty("bob"), is(true));
		assertThat(expressionFunction.isNotEmpty("  bob  "), is(true));
		assertThat(expressionFunction.isNotEmpty(123), is(true));
	}

	@Test
	public void testTrim() throws Exception {
		assertThat(expressionFunction.trim(null), nullValue());
		assertThat(expressionFunction.trim(""), is(""));
		assertThat(expressionFunction.trim(""), is(""));
		assertThat(expressionFunction.trim("     "), is(""));
		assertThat(expressionFunction.trim("abc"), is("abc"));
		assertThat(expressionFunction.trim("    abc    "), is("abc"));
	}

	@Test
	public void testTrimToEmpty() throws Exception {
		assertThat(expressionFunction.trimToEmpty(null), is(""));
		assertThat(expressionFunction.trimToEmpty(""), is(""));
		assertThat(expressionFunction.trimToEmpty(""), is(""));
		assertThat(expressionFunction.trimToEmpty("     "), is(""));
		assertThat(expressionFunction.trimToEmpty("abc"), is("abc"));
		assertThat(expressionFunction.trimToEmpty("    abc    "), is("abc"));
	}

	@Test
	public void testLeft() throws Exception {
		assertThat(expressionFunction.left(null, 1), nullValue());
		assertThat(expressionFunction.left("abc", -1), is(""));
		assertThat(expressionFunction.left("", 2), is(""));
		assertThat(expressionFunction.left("abc", 0), is(""));
		assertThat(expressionFunction.left("abc", 2), is("ab"));
		assertThat(expressionFunction.left("abc", 4), is("abc"));
	}

	@Test
	public void testRight() throws Exception {
		assertThat(expressionFunction.right(null, 1), nullValue());
		assertThat(expressionFunction.right("abc", -1), is(""));
		assertThat(expressionFunction.right("", 2), is(""));
		assertThat(expressionFunction.right("abc", 0), is(""));
		assertThat(expressionFunction.right("abc", 2), is("bc"));
		assertThat(expressionFunction.right("abc", 4), is("abc"));
	}

	@Test
	public void testMid() throws Exception {
		assertThat(expressionFunction.mid(null, 1, 2), nullValue());
		assertThat(expressionFunction.mid("abc", 1, -1), is(""));
		assertThat(expressionFunction.mid("", 0, 2), is(""));
		assertThat(expressionFunction.mid("abc", 0, 2), is("ab"));
		assertThat(expressionFunction.mid("abc", 0, 4), is("abc"));
		assertThat(expressionFunction.mid("abc", 2, 4), is("c"));
		assertThat(expressionFunction.mid("abc", 4, 2), is(""));
		assertThat(expressionFunction.mid("abc", -2, 2), is("ab"));
	}

	@Test
	public void testRightPad() throws Exception {
		assertThat(expressionFunction.rightPad(null, 3), nullValue());
		assertThat(expressionFunction.rightPad("", 3), is("   "));
		assertThat(expressionFunction.rightPad("bat", 3), is("bat"));
		assertThat(expressionFunction.rightPad("bat", 5), is("bat  "));
		assertThat(expressionFunction.rightPad("bat", 1), is("bat"));
		assertThat(expressionFunction.rightPad("bat", -1), is("bat"));
	}

	@Test
	public void testRightPadWithChar() throws Exception {
		assertThat(expressionFunction.rightPad(null, 3, ' '), nullValue());
		assertThat(expressionFunction.rightPad("", 3, 'z'), is("zzz"));
		assertThat(expressionFunction.rightPad("bat", 3, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad("bat", 5, 'z'), is("batzz"));
		assertThat(expressionFunction.rightPad("bat", 1, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	public void testLeftPad() throws Exception {
		assertThat(expressionFunction.leftPad(null, 3), nullValue());
		assertThat(expressionFunction.leftPad("", 3), is("   "));
		assertThat(expressionFunction.leftPad("bat", 3), is("bat"));
		assertThat(expressionFunction.leftPad("bat", 5), is("  bat"));
		assertThat(expressionFunction.leftPad("bat", 1), is("bat"));
		assertThat(expressionFunction.leftPad("bat", -1), is("bat"));
	}

	@Test
	public void testLeftPadWithChar() throws Exception {
		assertThat(expressionFunction.leftPad(null, 3, ' '), nullValue());
		assertThat(expressionFunction.leftPad("", 3, 'z'), is("zzz"));
		assertThat(expressionFunction.leftPad("bat", 3, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad("bat", 5, 'z'), is("zzbat"));
		assertThat(expressionFunction.leftPad("bat", 1, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	public void testSplit() throws Exception {
		assertThat(expressionFunction.split(null), nullValue());
		assertThat(expressionFunction.split(""), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("abc def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split("abc  def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split(" abc "), is(Matchers.arrayContaining("abc")));
	}

	@Test
	public void testSplitWithChar() throws Exception {
		assertThat(expressionFunction.split(null, '.'), nullValue());
		assertThat(expressionFunction.split("", '.'), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("a.b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split("a..b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split("a:b:c", '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(expressionFunction.split("a b c", ' '), is(Matchers.arrayContaining("a", "b", "c")));
	}

	@Test
	public void testSplitWithMax() throws Exception {
		assertThat(expressionFunction.split(null, ".", 2), nullValue());
		assertThat(expressionFunction.split("", ".", 2), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("ab cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab   cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab:cd:ef", ":", 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab:cd:ef", ":", 2), is(Matchers.arrayContaining("ab", "cd:ef")));
	}

	@Test
	public void testCapitalize() throws Exception {
		assertThat(expressionFunction.capitalize(null), nullValue());
		assertThat(expressionFunction.capitalize(""), is(""));
		assertThat(expressionFunction.capitalize("cat"), is("Cat"));
		assertThat(expressionFunction.capitalize("cAt"), is("CAt"));
	}

	@Test
	public void testUncapitalize() throws Exception {
		assertThat(expressionFunction.uncapitalize(null), nullValue());
		assertThat(expressionFunction.uncapitalize(""), is(""));
		assertThat(expressionFunction.uncapitalize("Cat"), is("cat"));
		assertThat(expressionFunction.uncapitalize("CAt"), is("cAt"));
	}

	@Test
	public void testIncrementShort() throws Exception {
		assertThat(expressionFunction.incrementShort((short) 1), is((short) 2));
		assertThat(expressionFunction.incrementShort(Short.MAX_VALUE), is(Short.MIN_VALUE));
	}

	@Test
	public void testIncrementInt() throws Exception {
		assertThat(expressionFunction.incrementInt(1), is(2));
		assertThat(expressionFunction.incrementInt(Integer.MAX_VALUE), is(Integer.MIN_VALUE));
	}

	@Test
	public void testIncrementLong() throws Exception {
		assertThat(expressionFunction.incrementLong(1L), is(2L));
		assertThat(expressionFunction.incrementLong(Long.MAX_VALUE), is(Long.MIN_VALUE));
	}

	@Test
	public void testIncrement() throws Exception {
		assertThat(expressionFunction.increment((short) 1), is((short) 2));
		assertThat(expressionFunction.increment(Short.MAX_VALUE), is(Short.MIN_VALUE));
		assertThat(expressionFunction.increment(1), is(2));
		assertThat(expressionFunction.increment(Integer.MAX_VALUE), is(Integer.MIN_VALUE));
		assertThat(expressionFunction.increment(1L), is(2L));
		assertThat(expressionFunction.increment(Long.MAX_VALUE), is(Long.MIN_VALUE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIncrementWithException() throws Exception {
		expressionFunction.increment(1f);
	}

}
