package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.SQLException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;

public class ProcedureEventTest extends AbstractDbTest {

	@BeforeEach
	public void setUpLocal() {
		agent.required(() -> {
			agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
			agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
					"String toUpperCase(String lower) throws Exception {\r\n" +
					"    return lower.toUpperCase();\r\n" +
					"}\r\n" +
					"$$;").count();
		});
	}

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterGetOutParameterListener(this::afterGetOutParameter);
				afterCreateCallableStatementListener(this::afterCreateCallableStatement);
				procedureListener(this::procedure);
			}

			void afterGetOutParameter(final AfterGetOutParameterEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getCallableStatement(), not(nullValue()));
				assertThat(event.getKey(), is("ret"));
				assertThat(event.getValue(), is("TEST1"));
				assertThat(event.getParameterIndex(), is(1));
			}

			void afterCreateCallableStatement(final AfterCreateCallableStatementEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
				assertThat(event.getCallableStatement(), not(nullValue()));
			}

			void procedure(final ProcedureEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
				assertThat(event.getCallableStatement(), not(nullValue()));
				assertThat(event.isResult(), is(true));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.required(() -> {
			try {
				var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
						.outParam("ret", JDBCType.VARCHAR)
						.param("param1", "test1").call();
				assertThat(ans.get("ret"), is("TEST1"));
			} catch (SQLException ex) {
				fail();
			}
		});

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyOutParameterEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterGetOutParameterListener(this::afterGetOutParameter);
			}

			void afterGetOutParameter(final AfterGetOutParameterEvent event) {
				event.setValue("MODIFIED");
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.required(() -> {
			try {
				var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
						.outParam("ret", JDBCType.VARCHAR)
						.param("param1", "test1").call();
				assertThat(ans.get("ret"), is("MODIFIED"));
			} catch (SQLException ex) {
				fail();
			}
		});

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyCallableStatementEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterCreateCallableStatementListener(this::afterCreateCallableStatement);
			}

			void afterCreateCallableStatement(final AfterCreateCallableStatementEvent event) {
				event.setCallableStatement(event.getCallableStatement());
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.required(() -> {
			try {
				var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
						.outParam("ret", JDBCType.VARCHAR)
						.param("param1", "test1")
						.call();
				assertThat(ans.get("ret"), is("TEST1"));
			} catch (SQLException ex) {
				fail();
			}
		});

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
