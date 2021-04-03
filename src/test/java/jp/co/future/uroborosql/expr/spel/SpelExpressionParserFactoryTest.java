package jp.co.future.uroborosql.expr.spel;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import org.junit.jupiter.api.Test;

public class SpelExpressionParserFactoryTest {

	@Test
	public void test() {
		SpelExpressionParserFactory factory = new SpelExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(SpelExpressionParser.class));
	}
}
