package jp.co.future.uroborosql.expr.ognl;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

public class OgnlExpressionParserFactoryTest {

	@Test
	void test() {
		var factory = new OgnlExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(OgnlExpressionParser.class));
	}
}
