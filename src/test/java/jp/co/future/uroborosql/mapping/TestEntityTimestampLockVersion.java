package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST_DATA_TIMESTAMP_LOCK_VERSION")
public class TestEntityTimestampLockVersion {
	private Long id;
	private String name;
	@Version(supplier = TimestampOptimisticLockSupplier.class)
	private long lockVersion = 0;

	public TestEntityTimestampLockVersion() {
	}

	public TestEntityTimestampLockVersion(final Long id, final String name) {
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

	public long getLockVersion() {
		return lockVersion;
	}

	public void setLockVersion(final long lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (id == null ? 0 : id.hashCode());
		result = prime * result + (int) (lockVersion ^ lockVersion >>> 32);
		result = prime * result + (name == null ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		TestEntityTimestampLockVersion other = (TestEntityTimestampLockVersion) obj;
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		if (lockVersion != other.lockVersion) {
			return false;
		}
		if (name == null) {
			if (other.name != null) {
				return false;
			}
		} else if (!name.equals(other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntityTimestampLockVersion [id=" + id + ", name=" + name + ", lockVersion=" + lockVersion + "]";
	}

}
