package jp.co.future.uroborosql.coverage;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.Arrays;

import org.junit.Test;

public class CoverageHandlerTest {

	@Test
	public void testParseLineRanges01() {
		assertThat(CoverageHandler.parseLineRanges("A\n  --   ELSE    \nB"), is(Arrays.asList(
				new Range(0, 0),
				new Range(17, 18))));
	}

	@Test
	public void testParseLineRanges02() {
		assertThat(CoverageHandler.parseLineRanges("A\n    \nB"), is(Arrays.asList(
				new Range(0, 0),
				new Range(6, 7))));
	}

}
