package jp.co.future.uroborosql.utils;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.dialect.DefaultDialect;
import ognl.Ognl;
import ognl.OgnlContext;

public class StringFunctionTest {
	private StringFunction expressionFunction;

	@BeforeEach
	public void setUp() throws Exception {
		expressionFunction = new StringFunction(new DefaultDialect());
	}

	@AfterEach
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws Exception {
		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val1", null);
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		Ognl.parseExpression("SF.isEmpty(val1)");

		assertThat((boolean) Ognl.getValue("SF.isEmpty(val1)", context, root, null), is(true));
	}

	@Test
	public void testStartsWith() throws Exception {
		assertThat(expressionFunction.startsWith("abc"), is("abc%"));
		assertThat(expressionFunction.startsWith("a_bc"), is("a$_bc%"));
		assertThat(expressionFunction.startsWith("a%bc"), is("a$%bc%"));
		assertThat(expressionFunction.startsWith(""), is("%"));
		assertThat(expressionFunction.startsWith(null), is("%"));
		assertThat(expressionFunction.startsWith(123), is("123%"));
		assertThat(expressionFunction.startsWith(Optional.empty()), is("%"));
		assertThat(expressionFunction.startsWith(Optional.of("")), is("%"));
		assertThat(expressionFunction.startsWith(Optional.of("abc")), is("abc%"));
		assertThat(expressionFunction.startsWith(Optional.of(123)), is("123%"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.startsWith(val)"), context, root), is("abc%"));
	}

	@Test
	public void testStartsWithNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> new StringFunction().startsWith("abc"));
	}

	@Test
	public void testContains() throws Exception {
		assertThat(expressionFunction.contains("abc"), is("%abc%"));
		assertThat(expressionFunction.contains("a_bc"), is("%a$_bc%"));
		assertThat(expressionFunction.contains("a%bc"), is("%a$%bc%"));
		assertThat(expressionFunction.contains(""), is("%"));
		assertThat(expressionFunction.contains(null), is("%"));
		assertThat(expressionFunction.contains(123), is("%123%"));
		assertThat(expressionFunction.contains(Optional.empty()), is("%"));
		assertThat(expressionFunction.contains(Optional.of("")), is("%"));
		assertThat(expressionFunction.contains(Optional.of("abc")), is("%abc%"));
		assertThat(expressionFunction.contains(Optional.of(123)), is("%123%"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.contains(val)"), context, root), is("%abc%"));
	}

	@Test
	public void testContainsNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> new StringFunction().contains("abc"));
	}

	@Test
	public void testEndsWith() throws Exception {
		assertThat(expressionFunction.endsWith("abc"), is("%abc"));
		assertThat(expressionFunction.endsWith("a_bc"), is("%a$_bc"));
		assertThat(expressionFunction.endsWith("a%bc"), is("%a$%bc"));
		assertThat(expressionFunction.endsWith(""), is("%"));
		assertThat(expressionFunction.endsWith(null), is("%"));
		assertThat(expressionFunction.endsWith(123), is("%123"));
		assertThat(expressionFunction.endsWith(Optional.empty()), is("%"));
		assertThat(expressionFunction.endsWith(Optional.of("")), is("%"));
		assertThat(expressionFunction.endsWith(Optional.of("abc")), is("%abc"));
		assertThat(expressionFunction.endsWith(Optional.of(123)), is("%123"));

		Map<Object, Object> root = new HashMap<>();
		OgnlContext context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(StringFunction.SHORT_NAME, expressionFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.endsWith(val)"), context, root), is("%abc"));
	}

	@Test
	public void testEndsWithNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> new StringFunction().endsWith("abc"));
	}

	@Test
	public void testIsBlank() throws Exception {
		assertThat(expressionFunction.isBlank(null), is(true));
		assertThat(expressionFunction.isBlank(""), is(true));
		assertThat(expressionFunction.isBlank(" "), is(true));
		assertThat(expressionFunction.isBlank("bob"), is(false));
		assertThat(expressionFunction.isBlank("  bob  "), is(false));
		assertThat(expressionFunction.isBlank(123), is(false));
		assertThat(expressionFunction.isBlank(Optional.empty()), is(true));
		assertThat(expressionFunction.isBlank(Optional.of("")), is(true));
		assertThat(expressionFunction.isBlank(Optional.of(" ")), is(true));
		assertThat(expressionFunction.isBlank(Optional.of("bob")), is(false));
		assertThat(expressionFunction.isBlank(Optional.of(123)), is(false));
	}

	@Test
	public void testIsEmpty() throws Exception {
		assertThat(expressionFunction.isEmpty(null), is(true));
		assertThat(expressionFunction.isEmpty(""), is(true));
		assertThat(expressionFunction.isEmpty(" "), is(false));
		assertThat(expressionFunction.isEmpty("bob"), is(false));
		assertThat(expressionFunction.isEmpty("  bob  "), is(false));
		assertThat(expressionFunction.isEmpty(123), is(false));
		assertThat(expressionFunction.isEmpty(Optional.empty()), is(true));
		assertThat(expressionFunction.isEmpty(Optional.of("")), is(true));
		assertThat(expressionFunction.isEmpty(Optional.of(" ")), is(false));
		assertThat(expressionFunction.isEmpty(Optional.of("bob")), is(false));
		assertThat(expressionFunction.isEmpty(Optional.of(123)), is(false));
	}

	@Test
	public void testIsNotBlank() throws Exception {
		assertThat(expressionFunction.isNotBlank(null), is(false));
		assertThat(expressionFunction.isNotBlank(""), is(false));
		assertThat(expressionFunction.isNotBlank(" "), is(false));
		assertThat(expressionFunction.isNotBlank("bob"), is(true));
		assertThat(expressionFunction.isNotBlank("  bob  "), is(true));
		assertThat(expressionFunction.isNotBlank(123), is(true));
		assertThat(expressionFunction.isNotBlank(Optional.empty()), is(false));
		assertThat(expressionFunction.isNotBlank(Optional.of("")), is(false));
		assertThat(expressionFunction.isNotBlank(Optional.of(" ")), is(false));
		assertThat(expressionFunction.isNotBlank(Optional.of("bob")), is(true));
		assertThat(expressionFunction.isNotBlank(Optional.of(123)), is(true));
	}

	@Test
	public void testIsNotEmpty() throws Exception {
		assertThat(expressionFunction.isNotEmpty(null), is(false));
		assertThat(expressionFunction.isNotEmpty(""), is(false));
		assertThat(expressionFunction.isNotEmpty(" "), is(true));
		assertThat(expressionFunction.isNotEmpty("bob"), is(true));
		assertThat(expressionFunction.isNotEmpty("  bob  "), is(true));
		assertThat(expressionFunction.isNotEmpty(123), is(true));
		assertThat(expressionFunction.isNotEmpty(Optional.empty()), is(false));
		assertThat(expressionFunction.isNotEmpty(Optional.of("")), is(false));
		assertThat(expressionFunction.isNotEmpty(Optional.of(" ")), is(true));
		assertThat(expressionFunction.isNotEmpty(Optional.of("bob")), is(true));
		assertThat(expressionFunction.isNotEmpty(Optional.of(123)), is(true));
	}

	@Test
	public void testTrim() throws Exception {
		assertThat(expressionFunction.trim(null), nullValue());
		assertThat(expressionFunction.trim(""), is(""));
		assertThat(expressionFunction.trim("     "), is(""));
		assertThat(expressionFunction.trim("abc"), is("abc"));
		assertThat(expressionFunction.trim("    abc    "), is("abc"));
		assertThat(expressionFunction.trim(Optional.empty()), is(nullValue()));
		assertThat(expressionFunction.trim(Optional.of("")), is(""));
		assertThat(expressionFunction.trim(Optional.of("    ")), is(""));
		assertThat(expressionFunction.trim(Optional.of("abc")), is("abc"));
		assertThat(expressionFunction.trim(Optional.of("   abc   ")), is("abc"));
		assertThat(expressionFunction.trim(Optional.of(123)), is("123"));
	}

	@Test
	public void testTrimToEmpty() throws Exception {
		assertThat(expressionFunction.trimToEmpty(null), is(""));
		assertThat(expressionFunction.trimToEmpty(""), is(""));
		assertThat(expressionFunction.trimToEmpty("     "), is(""));
		assertThat(expressionFunction.trimToEmpty("abc"), is("abc"));
		assertThat(expressionFunction.trimToEmpty("    abc    "), is("abc"));
		assertThat(expressionFunction.trimToEmpty(Optional.empty()), is(""));
		assertThat(expressionFunction.trimToEmpty(Optional.of("")), is(""));
		assertThat(expressionFunction.trimToEmpty(Optional.of(" ")), is(""));
		assertThat(expressionFunction.trimToEmpty(Optional.of("abc")), is("abc"));
		assertThat(expressionFunction.trimToEmpty(Optional.of("   abc  ")), is("abc"));
		assertThat(expressionFunction.trimToEmpty(Optional.of(123)), is("123"));
	}

	@Test
	public void testLeft() throws Exception {
		assertThat(expressionFunction.left(null, 1), nullValue());
		assertThat(expressionFunction.left("abc", -1), is(""));
		assertThat(expressionFunction.left("", 2), is(""));
		assertThat(expressionFunction.left("abc", 0), is(""));
		assertThat(expressionFunction.left("abc", 2), is("ab"));
		assertThat(expressionFunction.left("abc", 4), is("abc"));
		assertThat(expressionFunction.left(123, 4), is("123"));
		assertThat(expressionFunction.left(Optional.empty(), 4), is(nullValue()));
		assertThat(expressionFunction.left(Optional.of(""), 2), is(""));
		assertThat(expressionFunction.left(Optional.of("abc"), 0), is(""));
		assertThat(expressionFunction.left(Optional.of("abc"), 2), is("ab"));
		assertThat(expressionFunction.left(Optional.of("abc"), 4), is("abc"));
		assertThat(expressionFunction.left(Optional.of(123), 4), is("123"));
	}

	@Test
	public void testRight() throws Exception {
		assertThat(expressionFunction.right(null, 1), nullValue());
		assertThat(expressionFunction.right("abc", -1), is(""));
		assertThat(expressionFunction.right("", 2), is(""));
		assertThat(expressionFunction.right("abc", 0), is(""));
		assertThat(expressionFunction.right("abc", 2), is("bc"));
		assertThat(expressionFunction.right("abc", 4), is("abc"));
		assertThat(expressionFunction.right(123, 4), is("123"));
		assertThat(expressionFunction.right(Optional.empty(), 4), is(nullValue()));
		assertThat(expressionFunction.right(Optional.of(""), 2), is(""));
		assertThat(expressionFunction.right(Optional.of("abc"), 0), is(""));
		assertThat(expressionFunction.right(Optional.of("abc"), 2), is("bc"));
		assertThat(expressionFunction.right(Optional.of("abc"), 4), is("abc"));
		assertThat(expressionFunction.right(Optional.of(123), 4), is("123"));
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
		assertThat(expressionFunction.mid(123, -2, 2), is("12"));
		assertThat(expressionFunction.mid(Optional.empty(), 1, 2), is(nullValue()));
		assertThat(expressionFunction.mid(Optional.of("abc"), 1, -1), is(""));
		assertThat(expressionFunction.mid(Optional.of("abc"), 0, 4), is("abc"));
		assertThat(expressionFunction.mid(Optional.of(123), 0, 4), is("123"));
	}

	@Test
	public void testRightPad() throws Exception {
		assertThat(expressionFunction.rightPad(null, 3), nullValue());
		assertThat(expressionFunction.rightPad("", 3), is("   "));
		assertThat(expressionFunction.rightPad("bat", 3), is("bat"));
		assertThat(expressionFunction.rightPad("bat", 5), is("bat  "));
		assertThat(expressionFunction.rightPad("bat", 1), is("bat"));
		assertThat(expressionFunction.rightPad("bat", -1), is("bat"));
		assertThat(expressionFunction.rightPad(123, -1), is("123"));
		assertThat(expressionFunction.rightPad(Optional.empty(), 3), is(nullValue()));
		assertThat(expressionFunction.rightPad(Optional.of(""), 3), is("   "));
		assertThat(expressionFunction.rightPad(Optional.of("bat"), 3), is("bat"));
		assertThat(expressionFunction.rightPad(Optional.of(123), 5), is("123  "));
	}

	@Test
	public void testRightPadWithChar() throws Exception {
		assertThat(expressionFunction.rightPad(null, 3, ' '), nullValue());
		assertThat(expressionFunction.rightPad("", 3, 'z'), is("zzz"));
		assertThat(expressionFunction.rightPad("bat", 3, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad("bat", 5, 'z'), is("batzz"));
		assertThat(expressionFunction.rightPad("bat", 1, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad("bat", -1, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad(123, -1, 'z'), is("123"));
		assertThat(expressionFunction.rightPad(Optional.empty(), 3, 'z'), is(nullValue()));
		assertThat(expressionFunction.rightPad(Optional.of(""), 3, 'z'), is("zzz"));
		assertThat(expressionFunction.rightPad(Optional.of("bat"), 3, 'z'), is("bat"));
		assertThat(expressionFunction.rightPad(Optional.of(123), 5, 'z'), is("123zz"));
	}

	@Test
	public void testLeftPad() throws Exception {
		assertThat(expressionFunction.leftPad(null, 3), nullValue());
		assertThat(expressionFunction.leftPad("", 3), is("   "));
		assertThat(expressionFunction.leftPad("bat", 3), is("bat"));
		assertThat(expressionFunction.leftPad("bat", 5), is("  bat"));
		assertThat(expressionFunction.leftPad("bat", 1), is("bat"));
		assertThat(expressionFunction.leftPad("bat", -1), is("bat"));
		assertThat(expressionFunction.leftPad(123, -1), is("123"));
		assertThat(expressionFunction.leftPad(Optional.empty(), 3), is(nullValue()));
		assertThat(expressionFunction.leftPad(Optional.of(""), 3), is("   "));
		assertThat(expressionFunction.leftPad(Optional.of("bat"), 3), is("bat"));
		assertThat(expressionFunction.leftPad(Optional.of(123), 5), is("  123"));
	}

	@Test
	public void testLeftPadWithChar() throws Exception {
		assertThat(expressionFunction.leftPad(null, 3, ' '), nullValue());
		assertThat(expressionFunction.leftPad("", 3, 'z'), is("zzz"));
		assertThat(expressionFunction.leftPad("bat", 3, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad("bat", 5, 'z'), is("zzbat"));
		assertThat(expressionFunction.leftPad("bat", 1, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad("bat", -1, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad(123, -1, 'z'), is("123"));
		assertThat(expressionFunction.leftPad(Optional.empty(), 3, 'z'), is(nullValue()));
		assertThat(expressionFunction.leftPad(Optional.of(""), 3, 'z'), is("zzz"));
		assertThat(expressionFunction.leftPad(Optional.of("bat"), 3, 'z'), is("bat"));
		assertThat(expressionFunction.leftPad(Optional.of(123), 5, 'z'), is("zz123"));
	}

	@Test
	public void testSplit() throws Exception {
		assertThat(expressionFunction.split(null), nullValue());
		assertThat(expressionFunction.split(""), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("abc def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split("abc  def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split(" abc "), is(Matchers.arrayContaining("abc")));
		assertThat(expressionFunction.split(123), is(Matchers.arrayContaining("123")));
		assertThat(expressionFunction.split(Optional.empty()), nullValue());
		assertThat(expressionFunction.split(Optional.of("")), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split(Optional.of("abc def")), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split(Optional.of("abc  def")), is(Matchers.arrayContaining("abc", "def")));
		assertThat(expressionFunction.split(Optional.of(" abc ")), is(Matchers.arrayContaining("abc")));
		assertThat(expressionFunction.split(Optional.of(123)), is(Matchers.arrayContaining("123")));
	}

	@Test
	public void testSplitWithChar() throws Exception {
		assertThat(expressionFunction.split(null, '.'), nullValue());
		assertThat(expressionFunction.split("", '.'), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("a.b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split("a..b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split("a:b:c", '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(expressionFunction.split("a b c", ' '), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split(Optional.empty(), '.'), nullValue());
		assertThat(expressionFunction.split(Optional.of(""), '.'), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split(Optional.of("a.b.c"), '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split(Optional.of("a..b.c"), '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(expressionFunction.split(Optional.of("a:b:c"), '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(expressionFunction.split(Optional.of("a b c"), ' '), is(Matchers.arrayContaining("a", "b", "c")));
	}

	@Test
	public void testSplitWithMax() throws Exception {
		assertThat(expressionFunction.split(null, ".", 2), nullValue());
		assertThat(expressionFunction.split("", ".", 2), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split("ab cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab   cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab:cd:ef", ":", 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split("ab:cd:ef", ":", 2), is(Matchers.arrayContaining("ab", "cd:ef")));
		assertThat(expressionFunction.split(Optional.empty(), ".", 2), nullValue());
		assertThat(expressionFunction.split(Optional.of(""), ".", 2), is(Matchers.emptyArray()));
		assertThat(expressionFunction.split(Optional.of("ab cd ef"), null, 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split(Optional.of("ab   cd ef"), null, 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split(Optional.of("ab:cd:ef"), ":", 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(expressionFunction.split(Optional.of("ab:cd:ef"), ":", 2),
				is(Matchers.arrayContaining("ab", "cd:ef")));
	}

	@Test
	public void testCapitalize() throws Exception {
		assertThat(expressionFunction.capitalize(null), nullValue());
		assertThat(expressionFunction.capitalize(""), is(""));
		assertThat(expressionFunction.capitalize("cat"), is("Cat"));
		assertThat(expressionFunction.capitalize("cAt"), is("CAt"));
		assertThat(expressionFunction.capitalize(123), is("123"));
		assertThat(expressionFunction.capitalize(Optional.empty()), nullValue());
		assertThat(expressionFunction.capitalize(Optional.of("")), is(""));
		assertThat(expressionFunction.capitalize(Optional.of("cat")), is("Cat"));
		assertThat(expressionFunction.capitalize(Optional.of("cAt")), is("CAt"));
		assertThat(expressionFunction.capitalize(Optional.of(123)), is("123"));
	}

	@Test
	public void testUncapitalize() throws Exception {
		assertThat(expressionFunction.uncapitalize(null), nullValue());
		assertThat(expressionFunction.uncapitalize(""), is(""));
		assertThat(expressionFunction.uncapitalize("Cat"), is("cat"));
		assertThat(expressionFunction.uncapitalize("CAt"), is("cAt"));
		assertThat(expressionFunction.uncapitalize(123), is("123"));
		assertThat(expressionFunction.uncapitalize(Optional.empty()), nullValue());
		assertThat(expressionFunction.uncapitalize(Optional.of("")), is(""));
		assertThat(expressionFunction.uncapitalize(Optional.of("Cat")), is("cat"));
		assertThat(expressionFunction.uncapitalize(Optional.of("CAt")), is("cAt"));
		assertThat(expressionFunction.uncapitalize(Optional.of(123)), is("123"));
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

	@Test
	public void testIncrementWithException() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> expressionFunction.increment(1f));
	}

}
