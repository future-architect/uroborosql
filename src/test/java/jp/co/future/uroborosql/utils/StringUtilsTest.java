package jp.co.future.uroborosql.utils;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import org.hamcrest.Matchers;
import org.junit.Test;

public class StringUtilsTest {
	@Test
	public void testCapitalize() throws Exception {
		assertThat(StringUtils.capitalize(null), nullValue());
		assertThat(StringUtils.capitalize(""), is(""));
		assertThat(StringUtils.capitalize("cat"), is("Cat"));
		assertThat(StringUtils.capitalize("cAt"), is("CAt"));
		assertThat(StringUtils.capitalize("Cat"), is("Cat"));
		assertThat(StringUtils.capitalize("CAt"), is("CAt"));
		assertThat(StringUtils.capitalize("c"), is("C"));
		assertThat(StringUtils.capitalize("C"), is("C"));
	}

	@Test
	public void testUncapitalize() throws Exception {
		assertThat(StringUtils.uncapitalize(null), nullValue());
		assertThat(StringUtils.uncapitalize(""), is(""));
		assertThat(StringUtils.uncapitalize("Cat"), is("cat"));
		assertThat(StringUtils.uncapitalize("CAt"), is("cAt"));
		assertThat(StringUtils.uncapitalize("cat"), is("cat"));
		assertThat(StringUtils.uncapitalize("cAt"), is("cAt"));
		assertThat(StringUtils.uncapitalize("C"), is("c"));
		assertThat(StringUtils.uncapitalize("c"), is("c"));
	}

	@Test
	public void testIsBlank() throws Exception {
		assertThat(StringUtils.isBlank(null), is(true));
		assertThat(StringUtils.isBlank(""), is(true));
		assertThat(StringUtils.isBlank(" "), is(true));
		assertThat(StringUtils.isBlank("bob"), is(false));
		assertThat(StringUtils.isBlank("  bob  "), is(false));
	}

	@Test
	public void testIsEmpty() throws Exception {
		assertThat(StringUtils.isEmpty(null), is(true));
		assertThat(StringUtils.isEmpty(""), is(true));
		assertThat(StringUtils.isEmpty(" "), is(false));
		assertThat(StringUtils.isEmpty("bob"), is(false));
		assertThat(StringUtils.isEmpty("  bob  "), is(false));
	}

	@Test
	public void testIsNotBlank() throws Exception {
		assertThat(StringUtils.isNotBlank(null), is(false));
		assertThat(StringUtils.isNotBlank(""), is(false));
		assertThat(StringUtils.isNotBlank(" "), is(false));
		assertThat(StringUtils.isNotBlank("bob"), is(true));
		assertThat(StringUtils.isNotBlank("  bob  "), is(true));
	}

	@Test
	public void testIsNotEmpty() throws Exception {
		assertThat(StringUtils.isNotEmpty(null), is(false));
		assertThat(StringUtils.isNotEmpty(""), is(false));
		assertThat(StringUtils.isNotEmpty(" "), is(true));
		assertThat(StringUtils.isNotEmpty("bob"), is(true));
		assertThat(StringUtils.isNotEmpty("  bob  "), is(true));
	}

	@Test
	public void testIsNumeric() throws Exception {
		assertThat(StringUtils.isNumeric(null), is(false));
		assertThat(StringUtils.isNumeric(""), is(false));
		assertThat(StringUtils.isNumeric("   "), is(false));
		assertThat(StringUtils.isNumeric("123"), is(true));
		assertThat(StringUtils.isNumeric("\u0967\u0968\u0969"), is(true));
		assertThat(StringUtils.isNumeric("12 3"), is(false));
		assertThat(StringUtils.isNumeric("ab2c"), is(false));
		assertThat(StringUtils.isNumeric("12-3"), is(false));
		assertThat(StringUtils.isNumeric("12.3"), is(false));
		assertThat(StringUtils.isNumeric("-123"), is(false));
		assertThat(StringUtils.isNumeric("+123"), is(false));
	}

	@Test
	public void testLeft() throws Exception {
		assertThat(StringUtils.left(null, 1), nullValue());
		assertThat(StringUtils.left("abc", -1), is(""));
		assertThat(StringUtils.left("", 2), is(""));
		assertThat(StringUtils.left("abc", 0), is(""));
		assertThat(StringUtils.left("abc", 2), is("ab"));
		assertThat(StringUtils.left("abc", 4), is("abc"));
	}

	@Test
	public void testLeftPad() throws Exception {
		assertThat(StringUtils.leftPad(null, 3), nullValue());
		assertThat(StringUtils.leftPad("", 3), is("   "));
		assertThat(StringUtils.leftPad("bat", 3), is("bat"));
		assertThat(StringUtils.leftPad("bat", 5), is("  bat"));
		assertThat(StringUtils.leftPad("bat", 1), is("bat"));
		assertThat(StringUtils.leftPad("bat", -1), is("bat"));
	}

	@Test
	public void testLeftPadWithChar() throws Exception {
		assertThat(StringUtils.leftPad(null, 3, ' '), nullValue());
		assertThat(StringUtils.leftPad("", 3, 'z'), is("zzz"));
		assertThat(StringUtils.leftPad("bat", 3, 'z'), is("bat"));
		assertThat(StringUtils.leftPad("bat", 5, 'z'), is("zzbat"));
		assertThat(StringUtils.leftPad("bat", 1, 'z'), is("bat"));
		assertThat(StringUtils.leftPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	public void testLeftPadWithCharStr() throws Exception {
		assertThat(StringUtils.leftPad(null, 3, " "), nullValue());
		assertThat(StringUtils.leftPad("", 3, "z"), is("zzz"));
		assertThat(StringUtils.leftPad("bat", 3, "yz"), is("bat"));
		assertThat(StringUtils.leftPad("bat", 5, "yz"), is("yzbat"));
		assertThat(StringUtils.leftPad("bat", 8, "yz"), is("yzyzybat"));
		assertThat(StringUtils.leftPad("bat", 1, "yz"), is("bat"));
		assertThat(StringUtils.leftPad("bat", -1, "yz"), is("bat"));
		assertThat(StringUtils.leftPad("bat", 5, null), is("  bat"));
		assertThat(StringUtils.leftPad("bat", 5, ""), is("  bat"));
		assertThat(StringUtils.leftPad("bat", 8, "abcdef"), is("abcdebat"));
	}

	@Test
	public void testMid() throws Exception {
		assertThat(StringUtils.mid(null, 1, 2), nullValue());
		assertThat(StringUtils.mid("abc", 1, -1), is(""));
		assertThat(StringUtils.mid("", 0, 2), is(""));
		assertThat(StringUtils.mid("abc", 0, 2), is("ab"));
		assertThat(StringUtils.mid("abc", 0, 4), is("abc"));
		assertThat(StringUtils.mid("abc", 2, 4), is("c"));
		assertThat(StringUtils.mid("abc", 4, 2), is(""));
		assertThat(StringUtils.mid("abc", -2, 2), is("ab"));
	}

	@Test
	public void testRemoveEnd() throws Exception {
		assertThat(StringUtils.removeEnd(null, ".com"), nullValue());
		assertThat(StringUtils.removeEnd("abc", null), is("abc"));
		assertThat(StringUtils.removeEnd("www.domain.com", ".com."), is("www.domain.com"));
		assertThat(StringUtils.removeEnd("www.domain.com", ".com."), is("www.domain.com"));
		assertThat(StringUtils.removeEnd("www.domain.com", ".com"), is("www.domain"));
		assertThat(StringUtils.removeEnd("www.domain.com", "domain"), is("www.domain.com"));
		assertThat(StringUtils.removeEnd("abc", ""), is("abc"));
	}

	@Test
	public void testRepeat() throws Exception {
		assertThat(StringUtils.repeat('e', 0), is(""));
		assertThat(StringUtils.repeat('e', 3), is("eee"));
		assertThat(StringUtils.repeat('e', -2), is(""));
	}

	@Test
	public void testRight() throws Exception {
		assertThat(StringUtils.right(null, 1), nullValue());
		assertThat(StringUtils.right("abc", -1), is(""));
		assertThat(StringUtils.right("", 2), is(""));
		assertThat(StringUtils.right("abc", 0), is(""));
		assertThat(StringUtils.right("abc", 2), is("bc"));
		assertThat(StringUtils.right("abc", 4), is("abc"));
	}

	@Test
	public void testRightPad() throws Exception {
		assertThat(StringUtils.rightPad(null, 3), nullValue());
		assertThat(StringUtils.rightPad("", 3), is("   "));
		assertThat(StringUtils.rightPad("bat", 3), is("bat"));
		assertThat(StringUtils.rightPad("bat", 5), is("bat  "));
		assertThat(StringUtils.rightPad("bat", 1), is("bat"));
		assertThat(StringUtils.rightPad("bat", -1), is("bat"));
	}

	@Test
	public void testRightPadWithChar() throws Exception {
		assertThat(StringUtils.rightPad(null, 3, ' '), nullValue());
		assertThat(StringUtils.rightPad("", 3, 'z'), is("zzz"));
		assertThat(StringUtils.rightPad("bat", 3, 'z'), is("bat"));
		assertThat(StringUtils.rightPad("bat", 5, 'z'), is("batzz"));
		assertThat(StringUtils.rightPad("bat", 1, 'z'), is("bat"));
		assertThat(StringUtils.rightPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	public void testRightPadWithCharStr() throws Exception {
		assertThat(StringUtils.rightPad(null, 3, " "), nullValue());
		assertThat(StringUtils.rightPad("", 3, "z"), is("zzz"));
		assertThat(StringUtils.rightPad("bat", 3, "yz"), is("bat"));
		assertThat(StringUtils.rightPad("bat", 5, "yz"), is("batyz"));
		assertThat(StringUtils.rightPad("bat", 8, "yz"), is("batyzyzy"));
		assertThat(StringUtils.rightPad("bat", 1, "yz"), is("bat"));
		assertThat(StringUtils.rightPad("bat", -1, "yz"), is("bat"));
		assertThat(StringUtils.rightPad("bat", 5, null), is("bat  "));
		assertThat(StringUtils.rightPad("bat", 5, ""), is("bat  "));
		assertThat(StringUtils.rightPad("bat", 8, "abcdef"), is("batabcde"));
	}

	@Test
	public void testSplit() throws Exception {
		assertThat(StringUtils.split(null), nullValue());
		assertThat(StringUtils.split(""), is(Matchers.emptyArray()));
		assertThat(StringUtils.split("abc def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(StringUtils.split("abc  def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(StringUtils.split(" abc "), is(Matchers.arrayContaining("abc")));
	}

	@Test
	public void testSplitWithChar() throws Exception {
		assertThat(StringUtils.split(null, '.'), nullValue());
		assertThat(StringUtils.split("", '.'), is(Matchers.emptyArray()));
		assertThat(StringUtils.split("a.b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(StringUtils.split("a..b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(StringUtils.split("a:b:c", '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(StringUtils.split("a b c", ' '), is(Matchers.arrayContaining("a", "b", "c")));
	}

	@Test
	public void testSplitWithMax() throws Exception {
		assertThat(StringUtils.split(null, ".", 2), nullValue());
		assertThat(StringUtils.split("", ".", 2), is(Matchers.emptyArray()));
		assertThat(StringUtils.split("ab cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(StringUtils.split("ab   cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(StringUtils.split("ab:cd:ef", ":", 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(StringUtils.split("ab:cd:ef", ":", 2), is(Matchers.arrayContaining("ab", "cd:ef")));
	}

	@Test
	public void testTrim() throws Exception {
		assertThat(StringUtils.trim(null), nullValue());
		assertThat(StringUtils.trim(""), is(""));
		assertThat(StringUtils.trim(""), is(""));
		assertThat(StringUtils.trim("     "), is(""));
		assertThat(StringUtils.trim("abc"), is("abc"));
		assertThat(StringUtils.trim("    abc    "), is("abc"));
	}

	@Test
	public void testTrimToEmpty() throws Exception {
		assertThat(StringUtils.trimToEmpty(null), is(""));
		assertThat(StringUtils.trimToEmpty(""), is(""));
		assertThat(StringUtils.trimToEmpty(""), is(""));
		assertThat(StringUtils.trimToEmpty("     "), is(""));
		assertThat(StringUtils.trimToEmpty("abc"), is("abc"));
		assertThat(StringUtils.trimToEmpty("    abc    "), is("abc"));
	}

}
