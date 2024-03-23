package jp.co.future.uroborosql.utils;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

public class ObjectUtilsTest {
	@Test
	void testCapitalize() throws Exception {
		assertThat(ObjectUtils.capitalize(null), nullValue());
		assertThat(ObjectUtils.capitalize(""), is(""));
		assertThat(ObjectUtils.capitalize("cat"), is("Cat"));
		assertThat(ObjectUtils.capitalize("cAt"), is("CAt"));
		assertThat(ObjectUtils.capitalize("Cat"), is("Cat"));
		assertThat(ObjectUtils.capitalize("CAt"), is("CAt"));
		assertThat(ObjectUtils.capitalize("c"), is("C"));
		assertThat(ObjectUtils.capitalize("C"), is("C"));
	}

	@Test
	void testUncapitalize() throws Exception {
		assertThat(ObjectUtils.uncapitalize(null), nullValue());
		assertThat(ObjectUtils.uncapitalize(""), is(""));
		assertThat(ObjectUtils.uncapitalize("Cat"), is("cat"));
		assertThat(ObjectUtils.uncapitalize("CAt"), is("cAt"));
		assertThat(ObjectUtils.uncapitalize("cat"), is("cat"));
		assertThat(ObjectUtils.uncapitalize("cAt"), is("cAt"));
		assertThat(ObjectUtils.uncapitalize("C"), is("c"));
		assertThat(ObjectUtils.uncapitalize("c"), is("c"));
	}

	@Test
	void testIsBlank() throws Exception {
		assertThat(ObjectUtils.isBlank(null), is(true));
		assertThat(ObjectUtils.isBlank(""), is(true));
		assertThat(ObjectUtils.isBlank(" "), is(true));
		assertThat(ObjectUtils.isBlank("bob"), is(false));
		assertThat(ObjectUtils.isBlank("  bob  "), is(false));
	}

	@Test
	void testIsEmpty() throws Exception {
		assertThat(ObjectUtils.isEmpty(null), is(true));
		assertThat(ObjectUtils.isEmpty(""), is(true));
		assertThat(ObjectUtils.isEmpty(" "), is(false));
		assertThat(ObjectUtils.isEmpty("bob"), is(false));
		assertThat(ObjectUtils.isEmpty("  bob  "), is(false));
		assertThat(ObjectUtils.isEmpty(new String[0]), is(true));
		assertThat(ObjectUtils.isEmpty(new String[] { "a" }), is(false));
		assertThat(ObjectUtils.isEmpty(List.of()), is(true));
		assertThat(ObjectUtils.isEmpty(List.of("a")), is(false));
		assertThat(ObjectUtils.isEmpty(Map.of()), is(true));
		assertThat(ObjectUtils.isEmpty(Map.of("aa", "bb")), is(false));
		assertThat(ObjectUtils.isEmpty(Optional.empty()), is(true));
		assertThat(ObjectUtils.isEmpty(Optional.of(List.of())), is(true));
		assertThat(ObjectUtils.isEmpty(Optional.of(List.of("a"))), is(false));
	}

	@Test
	void testIsNotBlank() throws Exception {
		assertThat(ObjectUtils.isNotBlank(null), is(false));
		assertThat(ObjectUtils.isNotBlank(""), is(false));
		assertThat(ObjectUtils.isNotBlank(" "), is(false));
		assertThat(ObjectUtils.isNotBlank("bob"), is(true));
		assertThat(ObjectUtils.isNotBlank("  bob  "), is(true));
	}

	@Test
	void testIsNotEmpty() throws Exception {
		assertThat(ObjectUtils.isNotEmpty(null), is(false));
		assertThat(ObjectUtils.isNotEmpty(""), is(false));
		assertThat(ObjectUtils.isNotEmpty(" "), is(true));
		assertThat(ObjectUtils.isNotEmpty("bob"), is(true));
		assertThat(ObjectUtils.isNotEmpty("  bob  "), is(true));
		assertThat(ObjectUtils.isNotEmpty(new String[0]), is(false));
		assertThat(ObjectUtils.isNotEmpty(new String[] { "a" }), is(true));
		assertThat(ObjectUtils.isNotEmpty(List.of()), is(false));
		assertThat(ObjectUtils.isNotEmpty(List.of("a")), is(true));
		assertThat(ObjectUtils.isNotEmpty(Map.of()), is(false));
		assertThat(ObjectUtils.isNotEmpty(Map.of("aa", "bb")), is(true));
		assertThat(ObjectUtils.isNotEmpty(Optional.empty()), is(false));
		assertThat(ObjectUtils.isNotEmpty(Optional.of(List.of())), is(false));
		assertThat(ObjectUtils.isNotEmpty(Optional.of(List.of("a"))), is(true));
	}

	@Test
	void testIsNumeric() throws Exception {
		assertThat(ObjectUtils.isNumeric(null), is(false));
		assertThat(ObjectUtils.isNumeric(""), is(false));
		assertThat(ObjectUtils.isNumeric("   "), is(false));
		assertThat(ObjectUtils.isNumeric("123"), is(true));
		assertThat(ObjectUtils.isNumeric("\u0967\u0968\u0969"), is(true));
		assertThat(ObjectUtils.isNumeric("12 3"), is(false));
		assertThat(ObjectUtils.isNumeric("ab2c"), is(false));
		assertThat(ObjectUtils.isNumeric("12-3"), is(false));
		assertThat(ObjectUtils.isNumeric("12.3"), is(false));
		assertThat(ObjectUtils.isNumeric("-123"), is(false));
		assertThat(ObjectUtils.isNumeric("+123"), is(false));
	}

	@Test
	void testLeft() throws Exception {
		assertThat(ObjectUtils.left(null, 1), nullValue());
		assertThat(ObjectUtils.left("abc", -1), is(""));
		assertThat(ObjectUtils.left("", 2), is(""));
		assertThat(ObjectUtils.left("abc", 0), is(""));
		assertThat(ObjectUtils.left("abc", 2), is("ab"));
		assertThat(ObjectUtils.left("abc", 4), is("abc"));
	}

	@Test
	void testLeftPad() throws Exception {
		assertThat(ObjectUtils.leftPad(null, 3), nullValue());
		assertThat(ObjectUtils.leftPad("", 3), is("   "));
		assertThat(ObjectUtils.leftPad("bat", 3), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", 5), is("  bat"));
		assertThat(ObjectUtils.leftPad("bat", 1), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", -1), is("bat"));
	}

	@Test
	void testLeftPadWithChar() throws Exception {
		assertThat(ObjectUtils.leftPad(null, 3, ' '), nullValue());
		assertThat(ObjectUtils.leftPad("", 3, 'z'), is("zzz"));
		assertThat(ObjectUtils.leftPad("bat", 3, 'z'), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", 5, 'z'), is("zzbat"));
		assertThat(ObjectUtils.leftPad("bat", 1, 'z'), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	void testLeftPadWithCharStr() throws Exception {
		assertThat(ObjectUtils.leftPad(null, 3, " "), nullValue());
		assertThat(ObjectUtils.leftPad("", 3, "z"), is("zzz"));
		assertThat(ObjectUtils.leftPad("bat", 3, "yz"), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", 5, "yz"), is("yzbat"));
		assertThat(ObjectUtils.leftPad("bat", 8, "yz"), is("yzyzybat"));
		assertThat(ObjectUtils.leftPad("bat", 1, "yz"), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", -1, "yz"), is("bat"));
		assertThat(ObjectUtils.leftPad("bat", 5, null), is("  bat"));
		assertThat(ObjectUtils.leftPad("bat", 5, ""), is("  bat"));
		assertThat(ObjectUtils.leftPad("bat", 8, "abcdef"), is("abcdebat"));
	}

	@Test
	void testMid() throws Exception {
		assertThat(ObjectUtils.mid(null, 1, 2), nullValue());
		assertThat(ObjectUtils.mid("abc", 1, -1), is(""));
		assertThat(ObjectUtils.mid("", 0, 2), is(""));
		assertThat(ObjectUtils.mid("abc", 0, 2), is("ab"));
		assertThat(ObjectUtils.mid("abc", 0, 4), is("abc"));
		assertThat(ObjectUtils.mid("abc", 2, 4), is("c"));
		assertThat(ObjectUtils.mid("abc", 4, 2), is(""));
		assertThat(ObjectUtils.mid("abc", -2, 2), is("ab"));
	}

	@Test
	void testRemoveEnd() throws Exception {
		assertThat(ObjectUtils.removeEnd(null, ".com"), nullValue());
		assertThat(ObjectUtils.removeEnd("abc", null), is("abc"));
		assertThat(ObjectUtils.removeEnd("www.domain.com", ".com."), is("www.domain.com"));
		assertThat(ObjectUtils.removeEnd("www.domain.com", ".com."), is("www.domain.com"));
		assertThat(ObjectUtils.removeEnd("www.domain.com", ".com"), is("www.domain"));
		assertThat(ObjectUtils.removeEnd("www.domain.com", "domain"), is("www.domain.com"));
		assertThat(ObjectUtils.removeEnd("abc", ""), is("abc"));
	}

	@Test
	void testRepeat() throws Exception {
		assertThat(ObjectUtils.repeat('e', 0), is(""));
		assertThat(ObjectUtils.repeat('e', 3), is("eee"));
		assertThat(ObjectUtils.repeat('e', -2), is(""));
	}

	@Test
	void testRight() throws Exception {
		assertThat(ObjectUtils.right(null, 1), nullValue());
		assertThat(ObjectUtils.right("abc", -1), is(""));
		assertThat(ObjectUtils.right("", 2), is(""));
		assertThat(ObjectUtils.right("abc", 0), is(""));
		assertThat(ObjectUtils.right("abc", 2), is("bc"));
		assertThat(ObjectUtils.right("abc", 4), is("abc"));
	}

	@Test
	void testRightPad() throws Exception {
		assertThat(ObjectUtils.rightPad(null, 3), nullValue());
		assertThat(ObjectUtils.rightPad("", 3), is("   "));
		assertThat(ObjectUtils.rightPad("bat", 3), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", 5), is("bat  "));
		assertThat(ObjectUtils.rightPad("bat", 1), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", -1), is("bat"));
	}

	@Test
	void testRightPadWithChar() throws Exception {
		assertThat(ObjectUtils.rightPad(null, 3, ' '), nullValue());
		assertThat(ObjectUtils.rightPad("", 3, 'z'), is("zzz"));
		assertThat(ObjectUtils.rightPad("bat", 3, 'z'), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", 5, 'z'), is("batzz"));
		assertThat(ObjectUtils.rightPad("bat", 1, 'z'), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", -1, 'z'), is("bat"));
	}

	@Test
	void testRightPadWithCharStr() throws Exception {
		assertThat(ObjectUtils.rightPad(null, 3, " "), nullValue());
		assertThat(ObjectUtils.rightPad("", 3, "z"), is("zzz"));
		assertThat(ObjectUtils.rightPad("bat", 3, "yz"), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", 5, "yz"), is("batyz"));
		assertThat(ObjectUtils.rightPad("bat", 8, "yz"), is("batyzyzy"));
		assertThat(ObjectUtils.rightPad("bat", 1, "yz"), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", -1, "yz"), is("bat"));
		assertThat(ObjectUtils.rightPad("bat", 5, null), is("bat  "));
		assertThat(ObjectUtils.rightPad("bat", 5, ""), is("bat  "));
		assertThat(ObjectUtils.rightPad("bat", 8, "abcdef"), is("batabcde"));
	}

	@Test
	void testSplit() throws Exception {
		assertThat(ObjectUtils.split(null), nullValue());
		assertThat(ObjectUtils.split(""), is(Matchers.emptyArray()));
		assertThat(ObjectUtils.split("abc def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(ObjectUtils.split("abc  def"), is(Matchers.arrayContaining("abc", "def")));
		assertThat(ObjectUtils.split(" abc "), is(Matchers.arrayContaining("abc")));
	}

	@Test
	void testSplitWithChar() throws Exception {
		assertThat(ObjectUtils.split(null, '.'), nullValue());
		assertThat(ObjectUtils.split("", '.'), is(Matchers.emptyArray()));
		assertThat(ObjectUtils.split("a.b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(ObjectUtils.split("a..b.c", '.'), is(Matchers.arrayContaining("a", "b", "c")));
		assertThat(ObjectUtils.split("a:b:c", '.'), is(Matchers.arrayContaining("a:b:c")));
		assertThat(ObjectUtils.split("a b c", ' '), is(Matchers.arrayContaining("a", "b", "c")));
	}

	@Test
	void testSplitWithMax() throws Exception {
		assertThat(ObjectUtils.split(null, ".", 2), nullValue());
		assertThat(ObjectUtils.split("", ".", 2), is(Matchers.emptyArray()));
		assertThat(ObjectUtils.split("ab cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(ObjectUtils.split("ab   cd ef", null, 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(ObjectUtils.split("ab:cd:ef", ":", 0), is(Matchers.arrayContaining("ab", "cd", "ef")));
		assertThat(ObjectUtils.split("ab:cd:ef", ":", 2), is(Matchers.arrayContaining("ab", "cd:ef")));
	}

	@Test
	void testTrim() throws Exception {
		assertThat(ObjectUtils.trim(null), nullValue());
		assertThat(ObjectUtils.trim(""), is(""));
		assertThat(ObjectUtils.trim(""), is(""));
		assertThat(ObjectUtils.trim("     "), is(""));
		assertThat(ObjectUtils.trim("abc"), is("abc"));
		assertThat(ObjectUtils.trim("    abc    "), is("abc"));
	}

	@Test
	void testTrimToEmpty() throws Exception {
		assertThat(ObjectUtils.trimToEmpty(null), is(""));
		assertThat(ObjectUtils.trimToEmpty(""), is(""));
		assertThat(ObjectUtils.trimToEmpty(""), is(""));
		assertThat(ObjectUtils.trimToEmpty("     "), is(""));
		assertThat(ObjectUtils.trimToEmpty("abc"), is("abc"));
		assertThat(ObjectUtils.trimToEmpty("    abc    "), is("abc"));
	}

}
