package jp.co.future.uroborosql.mapping;

import java.time.OffsetDateTime;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST_TIMESTAMPTZ")
public class TestEntityOffsetDateTime {
	private Long id;
	private String name;
	@Version(supplier = TimestampOptimisticLockSupplier.class)
	private OffsetDateTime updDatetime;

	public TestEntityOffsetDateTime() {
	}

	public TestEntityOffsetDateTime(final Long id, final String name) {
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

	public OffsetDateTime getUpdDatetime() {
		return updDatetime;
	}

	public void setUpdDatetime(final OffsetDateTime updDatetime) {
		this.updDatetime = updDatetime;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((updDatetime == null) ? 0 : updDatetime.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		TestEntityOffsetDateTime other = (TestEntityOffsetDateTime) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (updDatetime == null) {
			if (other.updDatetime != null)
				return false;
		} else if (!updDatetime.equals(other.updDatetime))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "TestEntityOffsetDateTime [id=" + id + ", name=" + name + ", updDatetime=" + updDatetime + "]";
	}

}