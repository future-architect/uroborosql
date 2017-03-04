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
public class CaseFormatTest {

	@Test
	public void testToSnake() {
		assertEquals("SNAKE", CaseFormat.SnakeCase.convert("snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.SnakeCase.convert("convertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.SnakeCase.convert("ConvertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.SnakeCase.convert("convert_to_snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.SnakeCase.convert("CONVERT_TO_SNAKE"));
		assertEquals("TO_SNAKE", CaseFormat.SnakeCase.convert("toSnake"));
		assertEquals("", CaseFormat.SnakeCase.convert(null));
		assertEquals("", CaseFormat.SnakeCase.convert(""));
		assertEquals("", CaseFormat.SnakeCase.convert(" "));
	}

	@Test
	public void testToCamel() {
		assertEquals("camel", CaseFormat.CamelCase.convert("CAMEL"));
		assertEquals("convertToCamel", CaseFormat.CamelCase.convert("CONVERT_TO_CAMEL"));
		assertEquals("convertToCamel", CaseFormat.CamelCase.convert("convert_to_camel"));
		assertEquals("convertToCamel", CaseFormat.CamelCase.convert("convertToCamel"));
		assertEquals("convertToCamel", CaseFormat.CamelCase.convert("convert_to_camel_"));
		assertEquals("ToCamel", CaseFormat.CamelCase.convert("_TO_CAMEL"));
		assertEquals("ToCamel", CaseFormat.CamelCase.convert("_TO_CAMEL_"));
		assertEquals("", CaseFormat.CamelCase.convert(null));
		assertEquals("", CaseFormat.CamelCase.convert(""));
		assertEquals("", CaseFormat.CamelCase.convert(" "));
	}

}
