package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException.EntityProcKind;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.mapping.EntityHandler;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityQueryImpl<E> extends AbstractSqlFluent<SqlEntityQuery<E>> implements SqlEntityQuery<E> {
	private final EntityHandler<?> entityHandler;
	private final Class<? extends E> entityType;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param context SqlContext
	 * @param entityType エンティティタイプ
	 */
	SqlEntityQueryImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final SqlContext context,
			final Class<? extends E> entityType) {
		super(agent, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#collect()
	 */
	@Override
	public List<E> collect() {
		return stream().collect(Collectors.toList());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#first()
	 */
	@Override
	public Optional<E> first() {
		return stream().findFirst();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#stream()
	 */
	@Override
	public Stream<E> stream() {
		try {
			return this.entityHandler.doSelect(agent(), context(), entityType);
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}

	}

}
