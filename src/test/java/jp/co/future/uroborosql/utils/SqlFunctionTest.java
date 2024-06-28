package jp.co.future.uroborosql.utils;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

public class SqlFunctionTest {
	private SqlFunction sqlFunction;

	@BeforeEach
	public void setUp() throws Exception {
		sqlFunction = new SqlFunction(new DefaultDialect());
	}

	@AfterEach
	public void tearDown() throws Exception {
	}

	@Test
	void test() throws Exception {
		Map<Object, Object> root = new HashMap<>();
		var context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val1", null);
		root.put(SqlFunction.SHORT_NAME, sqlFunction);

		Ognl.parseExpression("SF.isEmpty(val1)");

		assertTrue((boolean) Ognl.getValue("SF.isEmpty(val1)", context, root, null));
	}

	@Test
	void testStartsWith() throws Exception {
		assertThat(sqlFunction.startsWith("abc"), is("abc%"));
		assertThat(sqlFunction.startsWith("a_bc"), is("a$_bc%"));
		assertThat(sqlFunction.startsWith("a%bc"), is("a$%bc%"));
		assertThat(sqlFunction.startsWith(""), is("%"));
		assertThat(sqlFunction.startsWith(null), is("%"));
		assertThat(sqlFunction.startsWith(123), is("123%"));
		assertThat(sqlFunction.startsWith(Optional.empty()), is("%"));
		assertThat(sqlFunction.startsWith(Optional.of("")), is("%"));
		assertThat(sqlFunction.startsWith(Optional.of("abc")), is("abc%"));
		assertThat(sqlFunction.startsWith(Optional.of(123)), is("123%"));

		Map<Object, Object> root = new HashMap<>();
		var context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(SqlFunction.SHORT_NAME, sqlFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.startsWith(val)"), context, root), is("abc%"));
	}

	@Test
	void testStartsWithNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> {
			new SqlFunction().startsWith("abc");
		});
	}

	@Test
	void testContains() throws Exception {
		assertThat(sqlFunction.contains("abc"), is("%abc%"));
		assertThat(sqlFunction.contains("a_bc"), is("%a$_bc%"));
		assertThat(sqlFunction.contains("a%bc"), is("%a$%bc%"));
		assertThat(sqlFunction.contains(""), is("%"));
		assertThat(sqlFunction.contains(null), is("%"));
		assertThat(sqlFunction.contains(123), is("%123%"));
		assertThat(sqlFunction.contains(Optional.empty()), is("%"));
		assertThat(sqlFunction.contains(Optional.of("")), is("%"));
		assertThat(sqlFunction.contains(Optional.of("abc")), is("%abc%"));
		assertThat(sqlFunction.contains(Optional.of(123)), is("%123%"));

		Map<Object, Object> root = new HashMap<>();
		var context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(SqlFunction.SHORT_NAME, sqlFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.contains(val)"), context, root), is("%abc%"));
	}

	@Test
	void testContainsNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> {
			new SqlFunction().contains("abc");
		});
	}

	@Test
	void testEndsWith() throws Exception {
		assertThat(sqlFunction.endsWith("abc"), is("%abc"));
		assertThat(sqlFunction.endsWith("a_bc"), is("%a$_bc"));
		assertThat(sqlFunction.endsWith("a%bc"), is("%a$%bc"));
		assertThat(sqlFunction.endsWith(""), is("%"));
		assertThat(sqlFunction.endsWith(null), is("%"));
		assertThat(sqlFunction.endsWith(123), is("%123"));
		assertThat(sqlFunction.endsWith(Optional.empty()), is("%"));
		assertThat(sqlFunction.endsWith(Optional.of("")), is("%"));
		assertThat(sqlFunction.endsWith(Optional.of("abc")), is("%abc"));
		assertThat(sqlFunction.endsWith(Optional.of(123)), is("%123"));

		Map<Object, Object> root = new HashMap<>();
		var context = (OgnlContext) Ognl.createDefaultContext(root);
		root.put("val", "abc");
		root.put(SqlFunction.SHORT_NAME, sqlFunction);

		assertThat(Ognl.getValue(Ognl.parseExpression("SF.endsWith(val)"), context, root), is("%abc"));
	}

	@Test
	void testEndsWithNoDialect() throws Exception {
		assertThrows(IllegalStateException.class, () -> {
			new SqlFunction().endsWith("abc");
		});
	}

	@Test
	void testIsBlank() throws Exception {
		assertThat(sqlFunction.isBlank(null), is(true));
		assertThat(sqlFunction.isBlank(""), is(true));
		assertThat(sqlFunction.isBlank(" "), is(true));
		assertThat(sqlFunction.isBlank("bob"), is(false));
		assertThat(sqlFunction.isBlank("  bob  "), is(false));
		assertThat(sqlFunction.isBlank(123), is(false));
		assertThat(sqlFunction.isBlank(Optional.empty()), is(true));
		assertThat(sqlFunction.isBlank(Optional.of("")), is(true));
		assertThat(sqlFunction.isBlank(Optional.of(" ")), is(true));
		assertThat(sqlFunction.isBlank(Optional.of("bob")), is(false));
		assertThat(sqlFunction.isBlank(Optional.of(123)), is(false));
	}

	@Test
	void testIsEmpty() throws Exception {
		assertThat(sqlFunction.isEmpty(null), is(true));
		assertThat(sqlFunction.isEmpty(""), is(true));
		assertThat(sqlFunction.isEmpty(" "), is(false));
		assertThat(sqlFunction.isEmpty("bob"), is(false));
		assertThat(sqlFunction.isEmpty("  bob  "), is(false));
		assertThat(sqlFunction.isEmpty(123), is(false));
		assertThat(sqlFunction.isEmpty(Optional.empty()), is(true));
		assertThat(sqlFunction.isEmpty(Optional.of("")), is(true));
		assertThat(sqlFunction.isEmpty(Optional.of(" ")), is(false));
		assertThat(sqlFunction.isEmpty(Optional.of("bob")), is(false));
		assertThat(sqlFunction.isEmpty(Optional.of(123)), is(false));
	}

	@Test
	void testIsNotBlank() throws Exception {
		assertThat(sqlFunction.isNotBlank(null), is(false));
		assertThat(sqlFunction.isNotBlank(""), is(false));
		assertThat(sqlFunction.isNotBlank(" "), is(false));
		assertThat(sqlFunction.isNotBlank("bob"), is(true));
		assertThat(sqlFunction.isNotBlank("  bob  "), is(true));
		assertThat(sqlFunction.isNotBlank(123), is(true));
		assertThat(sqlFunction.isNotBlank(Optional.empty()), is(false));
		assertThat(sqlFunction.isNotBlank(Optional.of("")), is(false));
		assertThat(sqlFunction.isNotBlank(Optional.of(" ")), is(false));
		assertThat(sqlFunction.isNotBlank(Optional.of("bob")), is(true));
		assertThat(sqlFunction.isNotBlank(Optional.of(123)), is(true));
	}

	@Test
	void testIsNotEmpty() throws Exception {
		assertThat(sqlFunction.isNotEmpty(null), is(false));
		assertThat(sqlFunction.isNotEmpty(""), is(false));
		assertThat(sqlFunction.isNotEmpty(" "), is(true));
		assertThat(sqlFunction.isNotEmpty("bob"), is(true));
		assertThat(sqlFunction.isNotEmpty("  bob  "), is(true));
		assertThat(sqlFunction.isNotEmpty(123), is(true));
		assertThat(sqlFunction.isNotEmpty(Optional.empty()), is(false));
		assertThat(sqlFunction.isNotEmpty(Optional.of("")), is(false));
		assertThat(sqlFunction.isNotEmpty(Optional.of(" ")), is(true));
		assertThat(sqlFunction.isNotEmpty(Optional.of("bob")), is(true));
		assertThat(sqlFunction.isNotEmpty(Optional.of(123)), is(true));
	}

	@Test
	void testTrim() throws Exception {
		assertThat(sqlFunction.trim(null), nullValue());
		assertThat(sqlFunction.trim(""), is(""));
		assertThat(sqlFunction.trim("     "), is(""));
		assertThat(sqlFunction.trim("abc"), is("abc"));
		assertThat(sqlFunction.trim("    abc    "), is("abc"));
		assertThat(sqlFunction.trim(Optional.empty()), is(nullValue()));
		assertThat(sqlFunction.trim(Optional.of("")), is(""));
		assertThat(sqlFunction.trim(Optional.of("    ")), is(""));
		assertThat(sqlFunction.trim(Optional.of("abc")), is("abc"));
		assertThat(sqlFunction.trim(Optional.of("   abc   ")), is("abc"));
		assertThat(sqlFunction.trim(Optional.of(123)), is("123"));
	}

	@Test
	void testTrimToEmpty() throws Exception {
		assertThat(sqlFunction.trimToEmpty(null), is(""));
		assertThat(sqlFunction.trimToEmpty(""), is(""));
		assertThat(sqlFunction.trimToEmpty("     "), is(""));
		assertThat(sqlFunction.trimToEmpty("abc"), is("abc"));
		assertThat(sqlFunction.trimToEmpty("    abc    "), is("abc"));
		assertThat(sqlFunction.trimToEmpty(Optional.empty()), is(""));
		assertThat(sqlFunction.trimToEmpty(Optional.of("")), is(""));
		assertThat(sqlFunction.trimToEmpty(Optional.of(" ")), is(""));
		assertThat(sqlFunction.trimToEmpty(Optional.of("abc")), is("abc"));
		assertThat(sqlFunction.trimToEmpty(Optional.of("   abc  ")), is("abc"));
		assertThat(sqlFunction.trimToEmpty(Optional.of(123)), is("123"));
	}

	@Test
	void testLeft() throws Exception {
		assertThat(sqlFunction.left(null, 1), nullValue());
		assertThat(sqlFunction.left("abc", -1), is(""));
		assertThat(sqlFunction.left("", 2), is(""));
		assertThat(sqlFunction.left("abc", 0), is(""));
		assertThat(sqlFunction.left("abc", 2), is("ab"));
		assertThat(sqlFunction.left("abc", 4), is("abc"));
		assertThat(sqlFunction.left(123, 4), is("123"));
		assertThat(sqlFunction.left(Optional.empty(), 4), is(nullValue()));
		assertThat(sqlFunction.left(Optional.of(""), 2), is(""));
		assertThat(sqlFunction.left(Optional.of("abc"), 0), is(""));
		assertThat(sqlFunction.left(Optional.of("abc"), 2), is("ab"));
		assertThat(sqlFunction.left(Optional.of("abc"), 4), is("abc"));
		assertThat(sqlFunction.left(Optional.of(123), 4), is("123"));
	}

	@Test
	void testRight() throws Exception {
		assertThat(sqlFunction.right(null, 1), nullValue());
		assertThat(sqlFunction.right("abc", -1), is(""));
		assertThat(sqlFunction.right("", 2), is(""));
		assertThat(sqlFunction.right("abc", 0), is(""));
		assertThat(sqlFunction.right("abc", 2), is("bc"));
		assertThat(sqlFunction.right("abc", 4), is("abc"));
		assertThat(sqlFunction.right(123, 4), is("123"));
		assertThat(sqlFunction.right(Optional.empty(), 4), is(nullValue()));
		assertThat(sqlFunction.right(Optional.of(""), 2), is(""));
		assertThat(sqlFunction.right(Optional.of("abc"), 0), is(""));
		assertThat(sqlFunction.right(Optional.of("abc"), 2), is("bc"));
		assertThat(sqlFunction.right(Optional.of("abc"), 4), is("abc"));
		assertThat(sqlFunction.right(Optional.of(123), 4), is("123"));
	}

	@Test
	void testMid() throws Exception {
		assertThat(sqlFunction.mid(null, 1, 2), nullValue());
		assertThat(sqlFunction.mid("abc", 1, -1), is(""));
		assertThat(sqlFunction.mid("", 0, 2), is(""));
		assertThat(sqlFunction.mid("abc", 0, 2), is("ab"));
		assertThat(sqlFunction.mid("abc", 0, 4), is("abc"));
		assertThat(sqlFunction.mid("abc", 2, 4), is("c"));
		assertThat(sqlFunction.mid("abc", 4, 2), is(""));
		assertThat(sqlFunction.mid("abc", -2, 2), is("ab"));
		assertThat(sqlFunction.mid(123, -2, 2), is("12"));
		assertThat(sqlFunction.mid(Optional.empty(), 1, 2), is(nullValue()));
		assertThat(sqlFunction.mid(Optional.of("abc"), 1, -1), is(""));
		assertThat(sqlFunction.mid(Optional.of("abc"), 0, 4), is("abc"));
		assertThat(sqlFunction.mid(Optional.of(123), 0, 4), is("123"));
	}

	@Test
	void testRightPad() throws Exception {
		assertThat(sqlFunction.rightPad(null, 3), nullValue());
		assertThat(sqlFunction.rightPad("", 3), is("   "));
		assertThat(sqlFunction.rightPad("bat", 3), is("bat"));
		assertThat(sqlFunction.rightPad("bat", 5), is("bat  "));
		assertThat(sqlFunction.rightPad("bat", 1), is("bat"));
		assertThat(sqlFunction.rightPad("bat", -1), is("bat"));
		assertThat(sqlFunction.rightPad(123, -1), is("123"));
		assertThat(sqlFunction.rightPad(Optional.empty(), 3), is(nullValue()));
		assertThat(sqlFunction.rightPad(Optional.of(""), 3), is("   "));
		assertThat(sqlFunction.rightPad(Optional.of("bat"), 3), is("bat"));
		assertThat(sqlFunction.rightPad(Optional.of(123), 5), is("123  "));
	}

	@Test
	void testRightPadWithChar() throws Exception {
		assertThat(sqlFunction.rightPad(null, 3, ' '), nullValue());
		assertThat(sqlFunction.rightPad("", 3, 'z'), is("zzz"));
		assertThat(sqlFunction.rightPad("bat", 3, 'z'), is("bat"));
		assertThat(sqlFunction.rightPad("bat", 5, 'z'), is("batzz"));
		assertThat(sqlFunction.rightPad("bat", 1, 'z'), is("bat"));
		assertThat(sqlFunction.rightPad("bat", -1, 'z'), is("bat"));
		assertThat(sqlFunction.rightPad(123, -1, 'z'), is("123"));
		assertThat(sqlFunction.rightPad(Optional.empty(), 3, 'z'), is(nullValue()));
		assertThat(sqlFunction.rightPad(Optional.of(""), 3, 'z'), is("zzz"));
		assertThat(sqlFunction.rightPad(Optional.of("bat"), 3, 'z'), is("bat"));
		assertThat(sqlFunction.rightPad(Optional.of(123), 5, 'z'), is("123zz"));
	}

	@Test
	void testLeftPad() throws Exception {
		assertThat(sqlFunction.leftPad(null, 3), nullValue());
		assertThat(sqlFunction.leftPad("", 3), is("   "));
		assertThat(sqlFunction.leftPad("bat", 3), is("bat"));
		assertThat(sqlFunction.leftPad("bat", 5), is("  bat"));
		assertThat(sqlFunction.leftPad("bat", 1), is("bat"));
		assertThat(sqlFunction.leftPad("bat", -1), is("bat"));
		assertThat(sqlFunction.leftPad(123, -1), is("123"));
		assertThat(sqlFunction.leftPad(Optional.empty(), 3), is(nullValue()));
		assertThat(sqlFunction.leftPad(Optional.of(""), 3), is("   "));
		assertThat(sqlFunction.leftPad(Optional.of("bat"), 3), is("bat"));
		assertThat(sqlFunction.leftPad(Optional.of(123), 5), is("  123"));
	}

	@Test
	void testLeftPadWithChar() throws Exception {
		assertThat(sqlFunction.leftPad(null, 3, ' '), nullValue());
		assertThat(sqlFunction.leftPad("", 3, 'z'), is("zzz"));
		assertThat(sqlFunction.leftPad("bat", 3, 'z'), is("bat"));
		assertThat(sqlFunction.leftPad("bat", 5, 'z'), is("zzbat"));
		assertThat(sqlFunction.leftPad("bat", 1, 'z'), is("bat"));
		assertThat(sqlFunction.leftPad("bat", -1, 'z'), is("bat"));
		assertThat(sqlFunction.leftPad(123, -1, 'z'), is("123"));
		assertThat(sqlFunction.leftPad(Optional.empty(), 3, 'z'), is(nullValue()));
		assertThat(sqlFunction.leftPad(Optional.of(""), 3, 'z'), is("zzz"));
		assertThat(sqlFunction.leftPad(Optional.of("bat"), 3, 'z'), is("bat"));
		assertThat(sqlFunction.leftPad(Optional.of(123), 5, 'z'), is("zz123"));
	}

	@Test
	void testSplit() throws Exception {
		assertThat(sqlFunction.split(null), nullValue());
		assertThat(sqlFunction.split(""), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split("abc def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(sqlFunction.split("abc  def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(sqlFunction.split(" abc "), is(Matchers.arrayContaining("abc")));
		assertThat(sqlFunction.split(123), is(Matchers.arrayContaining("123")));
		assertThat(sqlFunction.split(Optional.empty()), nullValue());
		assertThat(sqlFunction.split(Optional.of("")), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split(Optional.of("abc def")), is(Matchers.arrayContaining("abc", "def")));
		assertThat(sqlFunction.split(Optional.of("abc  def")), is(Matchers.arrayContaining("abc", "def")));
		assertThat(sqlFunction.split(Optional.of(" abc ")), is(Matchers.arrayContaining("abc")));
		assertThat(sqlFunction.split(Optional.of(123)), is(Matchers.arrayContaining("123")));
	}

	@Test
	void testSplitWithChar() throws Exception {
		assertThat(sqlFunction.split(null, '.'), nullValue());
		assertThat(sqlFunction.split("", '.'), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split("a.b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(sqlFunction.split("a..b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(sqlFunction.split("a:b:c", '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(sqlFunction.split("a b c", ' '), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(sqlFunction.split(Optional.empty(), '.'), nullValue());
		assertThat(sqlFunction.split(Optional.of(""), '.'), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split(Optional.of("a.b.c"), '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(sqlFunction.split(Optional.of("a..b.c"), '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(sqlFunction.split(Optional.of("a:b:c"), '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(sqlFunction.split(Optional.of("a b c"), ' '), is(Matchers.arrayContaining("a", "b", "c")));
	}

	@Test
	void testSplitWithMax() throws Exception {
		assertThat(sqlFunction.split(null, ".", 2), nullValue());
		assertThat(sqlFunction.split("", ".", 2), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split("ab cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split("ab   cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split("ab:cd:ef", ":", 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split("ab:cd:ef", ":", 2), is(Matchers.arrayContaining("ab", "cd:ef")));
		assertThat(sqlFunction.split(Optional.empty(), ".", 2), nullValue());
		assertThat(sqlFunction.split(Optional.of(""), ".", 2), is(Matchers.emptyArray()));
		assertThat(sqlFunction.split(Optional.of("ab cd ef"), null, 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split(Optional.of("ab   cd ef"), null, 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split(Optional.of("ab:cd:ef"), ":", 0),
				is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(sqlFunction.split(Optional.of("ab:cd:ef"), ":", 2),
				is(Matchers.arrayContaining("ab", "cd:ef")));
	}

	@Test
	void testCapitalize() throws Exception {
		assertThat(sqlFunction.capitalize(null), nullValue());
		assertThat(sqlFunction.capitalize(""), is(""));
		assertThat(sqlFunction.capitalize("cat"), is("Cat"));
		assertThat(sqlFunction.capitalize("cAt"), is("CAt"));
		assertThat(sqlFunction.capitalize(123), is("123"));
		assertThat(sqlFunction.capitalize(Optional.empty()), nullValue());
		assertThat(sqlFunction.capitalize(Optional.of("")), is(""));
		assertThat(sqlFunction.capitalize(Optional.of("cat")), is("Cat"));
		assertThat(sqlFunction.capitalize(Optional.of("cAt")), is("CAt"));
		assertThat(sqlFunction.capitalize(Optional.of(123)), is("123"));
	}

	@Test
	void testUncapitalize() throws Exception {
		assertThat(sqlFunction.uncapitalize(null), nullValue());
		assertThat(sqlFunction.uncapitalize(""), is(""));
		assertThat(sqlFunction.uncapitalize("Cat"), is("cat"));
		assertThat(sqlFunction.uncapitalize("CAt"), is("cAt"));
		assertThat(sqlFunction.uncapitalize(123), is("123"));
		assertThat(sqlFunction.uncapitalize(Optional.empty()), nullValue());
		assertThat(sqlFunction.uncapitalize(Optional.of("")), is(""));
		assertThat(sqlFunction.uncapitalize(Optional.of("Cat")), is("cat"));
		assertThat(sqlFunction.uncapitalize(Optional.of("CAt")), is("cAt"));
		assertThat(sqlFunction.uncapitalize(Optional.of(123)), is("123"));
	}

	@Test
	void testIncrementShort() throws Exception {
		assertThat(sqlFunction.incrementShort((short) 1), is((short) 2));
		assertThat(sqlFunction.incrementShort(Short.MAX_VALUE), is(Short.MIN_VALUE));
	}

	@Test
	void testIncrementInt() throws Exception {
		assertThat(sqlFunction.incrementInt(1), is(2));
		assertThat(sqlFunction.incrementInt(Integer.MAX_VALUE), is(Integer.MIN_VALUE));
	}

	@Test
	void testIncrementLong() throws Exception {
		assertThat(sqlFunction.incrementLong(1L), is(2L));
		assertThat(sqlFunction.incrementLong(Long.MAX_VALUE), is(Long.MIN_VALUE));
	}

	@Test
	void testIncrement() throws Exception {
		assertThat(sqlFunction.increment((short) 1), is((short) 2));
		assertThat(sqlFunction.increment(Short.MAX_VALUE), is(Short.MIN_VALUE));
		assertThat(sqlFunction.increment(1), is(2));
		assertThat(sqlFunction.increment(Integer.MAX_VALUE), is(Integer.MIN_VALUE));
		assertThat(sqlFunction.increment(1L), is(2L));
		assertThat(sqlFunction.increment(Long.MAX_VALUE), is(Long.MIN_VALUE));
	}

	@Test
	void testIncrementWithException() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {
			sqlFunction.increment(1f);
		});
	}

}
