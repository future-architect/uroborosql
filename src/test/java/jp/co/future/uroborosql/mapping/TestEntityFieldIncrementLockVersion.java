package jp.co.future.uroborosql.mapping;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

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
		return HashCodeBuilder.reflectionHashCode(this, true);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj, true);
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}
