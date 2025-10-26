/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * TriConsumerのテストクラス
 */
public class TriConsumerTest {

	@Test
	void testAccept() {
		List<String> results = new ArrayList<>();
		TriConsumer<String, Integer, Boolean> consumer = (s, i, b) -> {
			results.add(s + "_" + i + "_" + b);
		};

		consumer.accept("test", 42, true);
		assertThat(results.size(), is(1));
		assertThat(results.get(0), is("test_42_true"));
	}

	@Test
	void testAndThen() {
		List<String> results = new ArrayList<>();
		
		TriConsumer<String, Integer, Boolean> first = (s, i, b) -> {
			results.add("first_" + s);
		};
		
		TriConsumer<String, Integer, Boolean> second = (s, i, b) -> {
			results.add("second_" + i);
		};

		TriConsumer<String, Integer, Boolean> combined = first.andThen(second);
		combined.accept("test", 42, true);

		assertThat(results.size(), is(2));
		assertThat(results.get(0), is("first_test"));
		assertThat(results.get(1), is("second_42"));
	}

	@Test
	void testAndThen_WithNull() {
		TriConsumer<String, Integer, Boolean> consumer = (s, i, b) -> {
			// do nothing
		};

		assertThrows(NullPointerException.class, () -> {
			consumer.andThen(null);
		});
	}

	@Test
	void testAndThen_Chaining() {
		List<String> results = new ArrayList<>();
		
		TriConsumer<String, Integer, Boolean> first = (s, i, b) -> results.add("1");
		TriConsumer<String, Integer, Boolean> second = (s, i, b) -> results.add("2");
		TriConsumer<String, Integer, Boolean> third = (s, i, b) -> results.add("3");

		TriConsumer<String, Integer, Boolean> chained = first.andThen(second).andThen(third);
		chained.accept("test", 42, true);

		assertThat(results.size(), is(3));
		assertThat(results.get(0), is("1"));
		assertThat(results.get(1), is("2"));
		assertThat(results.get(2), is("3"));
	}
}