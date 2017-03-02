/**
 *
 */
package jp.co.future.uroborosql.utils;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * ConvertUtilsのテストクラス
 *
 * @author H.Sugimoto
 *
 */
public class ConvertUtilsTest {

	@Test
	public void testToSnake() {
		assertEquals("SNAKE", ConvertUtils.toSnake("snake"));
		assertEquals("CONVERT_TO_SNAKE", ConvertUtils.toSnake("convertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", ConvertUtils.toSnake("ConvertToSnake"));
		assertEquals("TO_SNAKE", ConvertUtils.toSnake("toSnake"));
		assertEquals("", ConvertUtils.toSnake(null));
		assertEquals("", ConvertUtils.toSnake(""));
		assertEquals("", ConvertUtils.toSnake(" "));
	}

	@Test
	public void testToCamel() {
		assertEquals("camel", ConvertUtils.toCamel("CAMEL"));
		assertEquals("convertToCamel", ConvertUtils.toCamel("CONVERT_TO_CAMEL"));
		assertEquals("convertToCamel", ConvertUtils.toCamel("convert_to_camel"));
		assertEquals("convertToCamel", ConvertUtils.toCamel("convert_to_camel_"));
		assertEquals("ToCamel", ConvertUtils.toCamel("_TO_CAMEL"));
		assertEquals("ToCamel", ConvertUtils.toCamel("_TO_CAMEL_"));
		assertEquals("", ConvertUtils.toCamel(null));
		assertEquals("", ConvertUtils.toCamel(""));
		assertEquals("", ConvertUtils.toCamel(" "));
	}

}
