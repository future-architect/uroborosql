package jp.co.future.uroborosql.coverage;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

public class RangesTest {

	@Test
	public void testNew() {
		var ranges = new Ranges(Arrays.asList(
				new Range(2, 3),
				new Range(4, 5)));
		assertThat(ranges, is(new Ranges(2, 5)));
	}

	@Test
	public void testToString() {
		var ranges = new Ranges(Arrays.asList(
				new Range(2, 3),
				new Range(5, 6)));
		assertThat(ranges.toString(), is("[[2..3], [5..6]]"));
	}

	@Test
	public void testMinus() {
		var ranges = new Ranges(0, 10);
		ranges.minus(Arrays.asList(
				new Range(2, 3),
				new Range(6, 6)));
		assertThat(ranges, is(new Ranges(Arrays.asList(
				new Range(0, 1),
				new Range(4, 5),
				new Range(7, 10)))));

		ranges = new Ranges(Arrays.asList(
				new Range(0, 1),
				new Range(3, 4)));
		ranges.minus(new Ranges(0, 1));
		assertThat(ranges, is(new Ranges(3, 4)));

		ranges = new Ranges(0, 10);
		ranges.minus(new Ranges(6, 10));
		assertThat(ranges, is(new Ranges(0, 5)));

		ranges = new Ranges(0, 10);
		ranges.minus(new Ranges(0, 5));
		assertThat(ranges, is(new Ranges(6, 10)));

		ranges = new Ranges(0, 10);
		ranges.minus(new Ranges(5, 11));
		assertThat(ranges, is(new Ranges(0, 4)));

		ranges = new Ranges(1, 10);
		ranges.minus(new Ranges(0, 5));
		assertThat(ranges, is(new Ranges(6, 10)));

		ranges = new Ranges(1, 10);
		ranges.minus(new Ranges(0, 11));
		assertThat(ranges, is(new Ranges()));
	}

	@Test
	public void testIntersect() {
		var ranges = new Ranges(Arrays.asList(
				new Range(0, 1),
				new Range(3, 5),
				new Range(7, 10)));
		ranges.intersect(Arrays.asList(
				new Range(1, 4),
				new Range(8, 10)));
		assertThat(ranges, is(new Ranges(Arrays.asList(
				new Range(1, 1),
				new Range(3, 4),
				new Range(8, 10)))));

		ranges = new Ranges(1, 10);
		ranges.intersect(new Ranges(0, 13));
		assertThat(ranges, is(new Ranges(1, 10)));

		ranges = new Ranges(1, 10);
		ranges.intersect(new Ranges(11, 13));
		assertThat(ranges, is(new Ranges()));

		ranges = new Ranges(Arrays.asList(
				new Range(5, 7),
				new Range(9, 10)));
		ranges.intersect(new Ranges(5, 7));
		assertThat(ranges, is(new Ranges(5, 7)));

		ranges = new Ranges(Arrays.asList(
				new Range(5, 7),
				new Range(9, 10)));
		ranges.intersect(new Ranges(Arrays.asList(
				new Range(4, 5),
				new Range(7, 8))));
		assertThat(ranges, is(new Ranges(Arrays.asList(
				new Range(5, 5),
				new Range(7, 7)))));
	}

}
