package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriberTest.Product;

public class TransactionEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObjRequired() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterBeginTransactionListener(this::afterBeginTransaction);
				beforeEndTransactionListener(this::beforeEndTransaction);
				beforeCommitListener(this::beforeCommit);
				afterCommitListener(this::afterCommit);
				beforeRollbackListener(this::beforeRollback);
				afterRollbackListener(this::afterRollback);
			}

			void afterBeginTransaction(final AfterBeginTransactionEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.isRequiredNew(), is(false));
				assertThat(event.getTransactionLevel(), is(1));
			}

			void beforeEndTransaction(final BeforeEndTransactionEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.isRequiredNew(), is(false));
				assertThat(event.getTransactionLevel(), is(1));
				assertThat(event.getResult(), instanceOf(List.class));
			}

			void beforeCommit(final BeforeCommitEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void afterCommit(final AfterCommitEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void beforeRollback(final BeforeRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void afterRollback(final AfterRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.required(() -> agent.query(Product.class)
				.collect());

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateEventObjCommit() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeCommitListener(this::beforeCommit);
				afterCommitListener(this::afterCommit);
			}

			void beforeCommit(final BeforeCommitEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void afterCommit(final AfterCommitEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.commit();

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateEventObjRollback() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeRollbackListener(this::beforeRollback);
				afterRollbackListener(this::afterRollback);
			}

			void beforeRollback(final BeforeRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void afterRollback(final AfterRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.rollback();

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateEventObjRollbackOnly() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeRollbackListener(this::beforeRollback);
				afterRollbackListener(this::afterRollback);
			}

			void beforeRollback(final BeforeRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}

			void afterRollback(final AfterRollbackEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		try (var newAgent = config.agent()) {
			newAgent.required(() -> {
				newAgent.query(Product.class)
						.collect();
				newAgent.setRollbackOnly();
			});
		}

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateEventObjResultNull() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEndTransactionListener(this::beforeEndTransaction);
			}

			void beforeEndTransaction(final BeforeEndTransactionEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.isRequiredNew(), is(false));
				assertThat(event.getTransactionLevel(), is(1));
				assertThat(event.getResult(), is(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.required(() -> {
			agent.query(Product.class)
					.collect();
		});

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateEventObjRequiresNew() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEndTransactionListener(this::beforeEndTransaction);
			}

			void beforeEndTransaction(final BeforeEndTransactionEvent event) {
				assertThat(event.getTransactionContext(), not(nullValue()));
				assertThat(event.isRequiredNew(), is(true));
				assertThat(event.getTransactionLevel(), is(1));
				assertThat(event.getResult(), instanceOf(List.class));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		agent.requiresNew(() -> agent.query(Product.class)
				.collect());

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
