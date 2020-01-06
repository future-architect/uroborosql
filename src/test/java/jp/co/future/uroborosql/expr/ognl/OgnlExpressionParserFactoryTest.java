package jp.co.future.uroborosql.expr.ognl;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

public class OgnlExpressionParserFactoryTest {

	@Test
	public void test() {
		OgnlExpressionParserFactory factory = new OgnlExpressionParserFactory();
		assertThat(factory.accept(), is(true));
		assertThat(factory.create(), instanceOf(OgnlExpressionParser.class));
	}
}
