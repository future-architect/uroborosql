/**
 *
 */
package jp.co.future.uroborosql.utils;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

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
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("convertToPascal"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("convert_to_pascal_"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("ConvertToPascal"));
		assertEquals("ConvertToPascal", CaseFormat.PASCAL_CASE.convert("convertToPascal"));
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
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("ConvertToCamel"));
		assertEquals("convertToCamel", CaseFormat.CAMEL_CASE.convert("convert_to_camel_"));
		assertEquals("ToCamel", CaseFormat.CAMEL_CASE.convert("_TO_CAMEL"));
		assertEquals("ToCamel", CaseFormat.CAMEL_CASE.convert("_TO_CAMEL_"));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(null));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(""));
		assertEquals("", CaseFormat.CAMEL_CASE.convert(" "));
	}

	@Test
	public void testToUpper() {
		assertEquals("SNAKE", CaseFormat.UPPER_CASE.convert("snake"));
		assertEquals("CONVERTTOSNAKE", CaseFormat.UPPER_CASE.convert("convertToSnake"));
		assertEquals("CONVERTTOSNAKE", CaseFormat.UPPER_CASE.convert("ConvertToSnake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_CASE.convert("convert_to_snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.UPPER_CASE.convert("CONVERT_TO_SNAKE"));
		assertEquals("SNAKE", CaseFormat.UPPER_CASE.convert("SNAKE"));
		assertEquals("TOSNAKE", CaseFormat.UPPER_CASE.convert("toSnake"));
		assertEquals("", CaseFormat.UPPER_CASE.convert(null));
		assertEquals("", CaseFormat.UPPER_CASE.convert(""));
		assertEquals("", CaseFormat.UPPER_CASE.convert(" "));
	}

	@Test
	public void testToLower() {
		assertEquals("snake", CaseFormat.LOWER_CASE.convert("snake"));
		assertEquals("converttosnake", CaseFormat.LOWER_CASE.convert("convertToSnake"));
		assertEquals("converttosnake", CaseFormat.LOWER_CASE.convert("ConvertToSnake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_CASE.convert("convert_to_snake"));
		assertEquals("convert_to_snake", CaseFormat.LOWER_CASE.convert("CONVERT_TO_SNAKE"));
		assertEquals("snake", CaseFormat.LOWER_CASE.convert("SNAKE"));
		assertEquals("tosnake", CaseFormat.LOWER_CASE.convert("toSnake"));
		assertEquals("", CaseFormat.LOWER_CASE.convert(null));
		assertEquals("", CaseFormat.LOWER_CASE.convert(""));
		assertEquals("", CaseFormat.LOWER_CASE.convert(" "));
	}

	@Test
	public void testNone() {
		assertEquals("snake", CaseFormat.NONE.convert("snake"));
		assertEquals("convertToSnake", CaseFormat.NONE.convert("convertToSnake"));
		assertEquals("ConvertToSnake", CaseFormat.NONE.convert("ConvertToSnake"));
		assertEquals("convert_to_snake", CaseFormat.NONE.convert("convert_to_snake"));
		assertEquals("CONVERT_TO_SNAKE", CaseFormat.NONE.convert("CONVERT_TO_SNAKE"));
		assertEquals("SNAKE", CaseFormat.NONE.convert("SNAKE"));
		assertEquals("toSnake", CaseFormat.NONE.convert("toSnake"));
		assertEquals("", CaseFormat.NONE.convert(null));
		assertEquals("", CaseFormat.NONE.convert(""));
		assertEquals("", CaseFormat.NONE.convert(" "));
	}

}
