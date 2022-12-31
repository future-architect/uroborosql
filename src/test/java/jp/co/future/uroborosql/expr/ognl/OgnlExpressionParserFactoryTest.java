package jp.co.future.uroborosql.expr.ognl;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class OgnlExpressionParserFactoryTest {

	@Test
	public void test() {
		var factory = new OgnlExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(OgnlExpressionParser.class));
	}
}
