package jp.co.future.uroborosql.coverage;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;

import org.junit.jupiter.api.Test;

public class CoverageHandlerTest {

	@Test
	void testParseLineRanges01() {
		assertThat(CoverageHandler.parseLineRanges("A\n  --   ELSE    \nB"), is(List.of(
				new Range(0, 0),
				new Range(17, 18))));
	}

	@Test
	void testParseLineRanges02() {
		assertThat(CoverageHandler.parseLineRanges("A\n    \nB"), is(List.of(
				new Range(0, 0),
				new Range(6, 7))));
	}

}
