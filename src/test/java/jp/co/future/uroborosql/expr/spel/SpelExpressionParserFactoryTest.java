package jp.co.future.uroborosql.expr.spel;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

public class SpelExpressionParserFactoryTest {

	@Test
	void test() {
		var factory = new SpelExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(SpelExpressionParser.class));
	}
}
