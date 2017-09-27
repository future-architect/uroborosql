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
	public void testToUpperSnake() {
		assertEquals("SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("convertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("ConvertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("convert_to_snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("CONVERT_TO_SNAKE"));
		assertEquals("SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("SNAKE"));
		assertEquals("TO_SNAKE", CaseFormat.UPPER_SNAKE_CASE.convert("toSnake"));
		assertEquals("", CaseFormat.UPPER_SNAKE_CASE.convert(null));
		assertEquals("", CaseFormat.UPPER_SNAKE_CASE.convert(""));
		assertEquals("", CaseFormat.UPPER_SNAKE_CASE.convert(" "));
	}

	@Test
	public void testToLowerSnake() {
		assertEquals("snake", CaseFormat.LOWER_SNAKE_CASE.convert("snake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_SNAKE_CASE.convert("convertToSnake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_SNAKE_CASE.convert("ConvertToSnake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_SNAKE_CASE.convert("convert_to_snake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_SNAKE_CASE.convert("CONVERT_TO_SNAKE"));
		assertEquals("snake", CaseFormat.LOWER_SNAKE_CASE.convert("SNAKE"));
		assertEquals("to_snake", CaseFormat.LOWER_SNAKE_CASE.convert("toSnake"));
		assertEquals("", CaseFormat.LOWER_SNAKE_CASE.convert(null));
		assertEquals("", CaseFormat.LOWER_SNAKE_CASE.convert(""));
		assertEquals("", CaseFormat.LOWER_SNAKE_CASE.convert(" "));
	}

	@Test
	public void testToPascal() {
		assertEquals("Pascal", CaseFormat.PASCAL_CASE.convert("PASCAL"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("CONVERT_TO_PASCAL"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("convert_to_pascal"));
		assertEquals("Converttopascal", CaseFormat.PASCAL_CASE.convert("convertToPascal"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("convert_to_pascal_"));
		assertEquals("ToPascal", CaseFormat.PASCAL_CASE.convert("_TO_PASCAL"));
		assertEquals("ToPascal", CaseFormat.PASCAL_CASE.convert("_TO_PASCAL_"));
		assertEquals("", CaseFormat.PASCAL_CASE.convert(null));
		assertEquals("", CaseFormat.PASCAL_CASE.convert(""));
		assertEquals("", CaseFormat.PASCAL_CASE.convert(" "));
	}

	@Test
	public void testToCamel() {
		assertEquals("camel", CaseFormat.CAMEL_CASE.convert("CAMEL"));
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("CONVERT_TO_CAMEL"));
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("convert_to_camel"));
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("convertToCamel"));
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("convert_to_camel_"));
		assertEquals("ToCamel", CaseFormat.CAMEL_CASE.convert("_TO_CAMEL"));
		assertEquals("ToCamel", CaseFormat.CAMEL_CASE.convert("_TO_CAMEL_"));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(null));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(""));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(" "));
	}

}
