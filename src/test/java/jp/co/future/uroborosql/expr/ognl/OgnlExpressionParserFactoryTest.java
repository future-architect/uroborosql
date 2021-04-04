package jp.co.future.uroborosql.expr.ognl;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

public class OgnlExpressionParserFactoryTest {

	@Test
	public void test() {
		var factory = new OgnlExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(OgnlExpressionParser.class));
	}
}
