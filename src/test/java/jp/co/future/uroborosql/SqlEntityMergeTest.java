package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Test;

import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

public class SqlEntityMergeTest extends AbstractDbTest {

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース。
	 */
	@Test
	public void testEntityMergeWithId() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(Optional.of("住所2_new"));

			assertThat(agent.merge(updateEntity), is(1));
			TestEntity result1 = agent.find(TestEntity.class, 2).orElse(null);
			assertThat(result1, not(nullValue()));
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.of("住所2_new")));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(Optional.of("住所10_new"))
					.setVersion(0);

			assertThat(agent.merge(insertEntity), is(1));
			TestEntity result2 = agent.find(TestEntity.class, 10).orElse(null);
			assertThat(result2, not(nullValue()));
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), is(Optional.of("住所10_new")));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース。
	 */
	@Test
	public void testEntityMergeAndReturnWithId() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(Optional.of("住所2_new"));

			TestEntity result1 = agent.mergeAndReturn(updateEntity);
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.of("住所2_new")));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(Optional.of("住所10_new"))
					.setVersion(0);

			TestEntity result2 = agent.mergeAndReturn(insertEntity);
			assertThat(result2.getId(), is(10));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), is(Optional.of("住所10_new")));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース(悲観ロックあり)。
	 */
	@Test
	public void testEntityMergeWithLockingWithId() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(Optional.of("住所2_new"));

			assertThat(agent.mergeWithLocking(updateEntity), is(1));
			TestEntity result1 = agent.find(TestEntity.class, 2).orElse(null);
			assertThat(result1, not(nullValue()));
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.of("住所2_new")));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(Optional.of("住所10_new"))
					.setVersion(0);

			assertThat(agent.mergeWithLocking(insertEntity), is(1));
			TestEntity result2 = agent.find(TestEntity.class, 10).orElse(null);
			assertThat(result2, not(nullValue()));
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), is(Optional.of("住所10_new")));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース（悲観ロックあり）。
	 */
	@Test
	public void testEntityMergeWithLockingAndReturnWithId() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(Optional.of("住所2_new"));

			TestEntity result1 = agent.mergeWithLockingAndReturn(updateEntity);
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.of("住所2_new")));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(Optional.of("住所10_new"))
					.setVersion(0);

			TestEntity result2 = agent.mergeWithLockingAndReturn(insertEntity);
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), is(Optional.of("住所10_new")));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース。Optional.empty()を設定した場合
	 */
	@Test
	public void testEntityMergeWithIdOptionalEmpty() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(Optional.empty());

			TestEntity result1 = agent.mergeAndReturn(updateEntity);
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.empty()));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(Optional.empty())
					.setVersion(0);

			TestEntity result2 = agent.mergeAndReturn(insertEntity);
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), is(Optional.empty()));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * @IdをもつEntityを使ったマージ処理のテストケース。Optionalフィールドにnullを設定した場合
	 */
	@Test
	public void testEntityMergeWithIdOptionalNull() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity cascade").count();
			agent.updateWith(
					"create table if not exists test_entity (id serial not null, name text not null, address text, version integer not null, primary key (id))")
					.count();

			List<TestEntity> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntity()
							.setName("名前" + i)
							.setAddress(Optional.of("住所" + i))
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntity.class, entities.stream()), is(9));

			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(null);

			TestEntity result1 = agent.mergeAndReturn(updateEntity);
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getAddress(), is(Optional.of("住所2"))); // 更新されないこと
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntity insertEntity = new TestEntity()
					.setName("名前10_new")
					.setAddress(null)
					.setVersion(0);

			TestEntity result2 = agent.mergeAndReturn(insertEntity);
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getAddress(), nullValue());
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	/**
	 * mergeの引数がStream型の場合は例外がスローされることを確認
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEntityMergeThrowWhenStreamParam() throws Exception {
		agent.required(() -> {
			TestEntity updateEntity = new TestEntity()
					.setId(2)
					.setName("名前2_new")
					.setAddress(null);

			List<TestEntity> input = new ArrayList<>();
			input.add(updateEntity);

			agent.merge(input.stream());
		});
	}

	/**
	 * 複合キーを持つEntityを使った一括マージ処理のテストケース。
	 */
	@Test
	public void testEntityMergesMultiKey() throws Exception {
		agent.required(() -> {
			// テーブル作成
			agent.updateWith("drop table if exists test_entity_multi_key cascade").count();
			agent.updateWith(
					"create table if not exists test_entity_multi_key (id integer not null, end_at timestamp with time zone not null, name text not null, address text, version integer not null, primary key (id, end_at))")
					.count();

			List<TestEntityMultiKey> entities = IntStream.range(1, 10)
					.mapToObj(i -> new TestEntityMultiKey()
							.setId(i)
							.setEndAt(LocalDate.now().plusDays(i))
							.setName("名前" + i)
							.setVersion(0))
					.collect(Collectors.toList());
			assertThat(agent.inserts(TestEntityMultiKey.class, entities.stream()), is(9));

			TestEntityMultiKey beforeEntity = entities.get(1);
			TestEntityMultiKey updateEntity = new TestEntityMultiKey()
					.setId(beforeEntity.getId())
					.setEndAt(beforeEntity.getEndAt())
					.setName(beforeEntity.getName() + "_new")
					.setVersion(beforeEntity.getVersion());

			TestEntityMultiKey result1 = agent.mergeAndReturn(updateEntity);
			assertThat(result1.getId(), is(updateEntity.getId()));
			assertThat(result1.getEndAt(), is(updateEntity.getEndAt()));
			assertThat(result1.getName(), is(updateEntity.getName()));
			assertThat(result1.getVersion(), is(updateEntity.getVersion() + 1));

			TestEntityMultiKey insertEntity = new TestEntityMultiKey()
					.setId(11)
					.setEndAt(LocalDate.now().plusDays(11))
					.setName("名前11_new")
					.setVersion(0);

			TestEntityMultiKey result2 = agent.mergeAndReturn(insertEntity);
			assertThat(result2.getId(), is(insertEntity.getId()));
			assertThat(result2.getEndAt(), is(insertEntity.getEndAt()));
			assertThat(result2.getName(), is(insertEntity.getName()));
			assertThat(result2.getVersion(), is(insertEntity.getVersion()));
		});
	}

	@Table(name = "test_entity")
	public static class TestEntity {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private int id;
		private String name;
		private Optional<String> address;
		@Version
		private int version;

		public TestEntity() {
		}

		public int getId() {
			return id;
		}

		public TestEntity setId(final int id) {
			this.id = id;
			return this;
		}

		public String getName() {
			return name;
		}

		public TestEntity setName(final String name) {
			this.name = name;
			return this;
		}

		public Optional<String> getAddress() {
			return this.address;
		}

		public TestEntity setAddress(final Optional<String> address) {
			this.address = address;
			return this;
		}

		public int getVersion() {
			return version;
		}

		public TestEntity setVersion(final int version) {
			this.version = version;
			return this;
		}
	}

	@Table(name = "test_entity_multi_key")
	public static class TestEntityMultiKey {
		private int id;
		private LocalDate endAt;
		private String name;
		private Optional<String> address;
		@Version
		private int version;

		public TestEntityMultiKey() {
		}

		public int getId() {
			return id;
		}

		public TestEntityMultiKey setId(final int id) {
			this.id = id;
			return this;
		}

		public LocalDate getEndAt() {
			return endAt;
		}

		public TestEntityMultiKey setEndAt(final LocalDate endAt) {
			this.endAt = endAt;
			return this;
		}

		public String getName() {
			return name;
		}

		public TestEntityMultiKey setName(final String name) {
			this.name = name;
			return this;
		}

		public Optional<String> getAddress() {
			return this.address;
		}

		public TestEntityMultiKey setAddress(final Optional<String> address) {
			this.address = address;
			return this;
		}

		public int getVersion() {
			return version;
		}

		public TestEntityMultiKey setVersion(final int version) {
			this.version = version;
			return this;
		}
	}

}
