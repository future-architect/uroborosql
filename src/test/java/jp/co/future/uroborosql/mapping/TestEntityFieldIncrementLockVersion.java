package jp.co.future.uroborosql.mapping;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST_DATA_FIELD_INCREMENT_LOCK_VERSION")
public class TestEntityFieldIncrementLockVersion {
	private Long id;
	private String name;
	@Version(supplier = FieldIncrementOptimisticLockSupplier.class)
	private short lockVersion = 0;

	public TestEntityFieldIncrementLockVersion() {
	}

	public TestEntityFieldIncrementLockVersion(final Long id, final String name) {
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

	public short getLockVersion() {
		return lockVersion;
	}

	public void setLockVersion(final short lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (id == null ? 0 : id.hashCode());
		result = prime * result + lockVersion;
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
		TestEntityFieldIncrementLockVersion other = (TestEntityFieldIncrementLockVersion) obj;
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
		return "TestEntityFieldIncrementLockVersion [id=" + id + ", name=" + name + ", lockVersion=" + lockVersion
				+ "]";
	}

}
