/**
 *
 */
package jp.co.future.uroborosql.utils;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("snake"), is("SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("convertToSnake"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("ConvertToSnake"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("convert_to_snake"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("CONVERT_TO_SNAKE"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("SNAKE"), is("SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert("toSnake"), is("TO_SNAKE"));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert(null), is(""));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert(""), is(""));
		assertThat(CaseFormat.UPPER_SNAKE_CASE.convert(" "), is(""));
	}

	@Test
	public void testToLowerSnake() {
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("snake"), is("snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("convertToSnake"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("ConvertToSnake"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("convert_to_snake"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("CONVERT_TO_SNAKE"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("SNAKE"), is("snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert("toSnake"), is("to_snake"));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert(null), is(""));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert(""), is(""));
		assertThat(CaseFormat.LOWER_SNAKE_CASE.convert(" "), is(""));
	}

	@Test
	public void testToPascal() {
		assertThat(CaseFormat.PASCAL_CASE.convert("PASCAL"), is("Pascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("CONVERT_TO_PASCAL"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("convert_to_pascal"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("convertToPascal"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("convert_to_pascal_"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("ConvertToPascal"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("convertToPascal"), is("ConvertToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("_TO_PASCAL"), is("ToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert("_TO_PASCAL_"), is("ToPascal"));
		assertThat(CaseFormat.PASCAL_CASE.convert(null), is(""));
		assertThat(CaseFormat.PASCAL_CASE.convert(""), is(""));
		assertThat(CaseFormat.PASCAL_CASE.convert(" "), is(""));
	}

	@Test
	public void testToCamel() {
		assertThat(CaseFormat.CAMEL_CASE.convert("CAMEL"), is("camel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("CONVERT_TO_CAMEL"), is("convertToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("convert_to_camel"), is("convertToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("convertToCamel"), is("convertToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("ConvertToCamel"), is("convertToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("convert_to_camel_"), is("convertToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("_TO_CAMEL"), is("ToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert("_TO_CAMEL_"), is("ToCamel"));
		assertThat(CaseFormat.CAMEL_CASE.convert(null), is(""));
		assertThat(CaseFormat.CAMEL_CASE.convert(""), is(""));
		assertThat(CaseFormat.CAMEL_CASE.convert(" "), is(""));
	}

	@Test
	public void testToUpper() {
		assertThat(CaseFormat.UPPER_CASE.convert("snake"), is("SNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("convertToSnake"), is("CONVERTTOSNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("ConvertToSnake"), is("CONVERTTOSNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("convert_to_snake"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("CONVERT_TO_SNAKE"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("SNAKE"), is("SNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert("toSnake"), is("TOSNAKE"));
		assertThat(CaseFormat.UPPER_CASE.convert(null), is(""));
		assertThat(CaseFormat.UPPER_CASE.convert(""), is(""));
		assertThat(CaseFormat.UPPER_CASE.convert(" "), is(""));
	}

	@Test
	public void testToLower() {
		assertThat(CaseFormat.LOWER_CASE.convert("snake"), is("snake"));
		assertThat(CaseFormat.LOWER_CASE.convert("convertToSnake"), is("converttosnake"));
		assertThat(CaseFormat.LOWER_CASE.convert("ConvertToSnake"), is("converttosnake"));
		assertThat(CaseFormat.LOWER_CASE.convert("convert_to_snake"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_CASE.convert("CONVERT_TO_SNAKE"), is("convert_to_snake"));
		assertThat(CaseFormat.LOWER_CASE.convert("SNAKE"), is("snake"));
		assertThat(CaseFormat.LOWER_CASE.convert("toSnake"), is("tosnake"));
		assertThat(CaseFormat.LOWER_CASE.convert(null), is(""));
		assertThat(CaseFormat.LOWER_CASE.convert(""), is(""));
		assertThat(CaseFormat.LOWER_CASE.convert(" "), is(""));
	}

	@Test
	public void testNone() {
		assertThat(CaseFormat.NONE.convert("snake"), is("snake"));
		assertThat(CaseFormat.NONE.convert("convertToSnake"), is("convertToSnake"));
		assertThat(CaseFormat.NONE.convert("ConvertToSnake"), is("ConvertToSnake"));
		assertThat(CaseFormat.NONE.convert("convert_to_snake"), is("convert_to_snake"));
		assertThat(CaseFormat.NONE.convert("CONVERT_TO_SNAKE"), is("CONVERT_TO_SNAKE"));
		assertThat(CaseFormat.NONE.convert("SNAKE"), is("SNAKE"));
		assertThat(CaseFormat.NONE.convert("toSnake"), is("toSnake"));
		assertThat(CaseFormat.NONE.convert(null), is(""));
		assertThat(CaseFormat.NONE.convert(""), is(""));
		assertThat(CaseFormat.NONE.convert(" "), is(""));
	}

}
