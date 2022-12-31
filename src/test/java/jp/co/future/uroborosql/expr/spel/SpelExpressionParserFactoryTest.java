package jp.co.future.uroborosql.expr.spel;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class SpelExpressionParserFactoryTest {

	@Test
	public void test() {
		var factory = new SpelExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(SpelExpressionParser.class));
	}
}
