package jp.co.future.uroborosql.expr.spel;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

public class SpelExpressionParserFactoryTest {

	@Test
	public void test() {
		SpelExpressionParserFactory factory = new SpelExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(SpelExpressionParser.class));
	}
}
