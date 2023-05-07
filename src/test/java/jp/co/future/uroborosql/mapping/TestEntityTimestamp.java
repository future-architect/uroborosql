package jp.co.future.uroborosql.mapping;

import java.sql.Timestamp;
import java.util.Objects;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST_TIMESTAMP")
public class TestEntityTimestamp {
	private Long id;
	private String name;
	@Version(supplier = TimestampOptimisticLockSupplier.class)
	private Timestamp updDatetime;

	public TestEntityTimestamp() {
	}

	public TestEntityTimestamp(final Long id, final String name) {
		this.id = id;
		this.name = name;
	}

	public Long getId() {
		return this.id;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	/**
	 * name を取得します。
	 *
	 * @return name
	 */
	public String getName() {
		return name;
	}

	/**
	 * name を設定します。
	 *
	 * @param name name
	 */
	public void setName(final String name) {
		this.name = name;
	}

	public Timestamp getUpdDatetime() {
		return updDatetime;
	}

	public void setUpdDatetime(final Timestamp updDatetime) {
		this.updDatetime = updDatetime;
	}

	@Override
	public int hashCode() {
		return Objects.hash(id, name, updDatetime);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		var other = (TestEntityTimestamp) obj;
		if (!Objects.equals(id, other.id) || !Objects.equals(name, other.name) || !Objects.equals(updDatetime, other.updDatetime)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntityTimestamp [id=" + id + ", name=" + name + ", updDatetime=" + updDatetime + "]";
	}

}