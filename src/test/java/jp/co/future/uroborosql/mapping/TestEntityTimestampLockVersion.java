package jp.co.future.uroborosql.mapping;

import java.util.Objects;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST_DATA_TIMESTAMP_LOCK_VERSION")
public class TestEntityTimestampLockVersion {
	private Long id;
	private String name;
	@Version(supplier = TimestampOptimisticLockSupplier.class)
	private Long lockVersion = 0L;

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

	public Long getLockVersion() {
		return lockVersion;
	}

	public void setLockVersion(final Long lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		return Objects.hash(id, lockVersion, name);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		var other = (TestEntityTimestampLockVersion) obj;
		if (!Objects.equals(id, other.id) || !lockVersion.equals(other.lockVersion) || !Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntityTimestampLockVersion [id=" + id + ", name=" + name + ", lockVersion=" + lockVersion + "]";
	}

}
