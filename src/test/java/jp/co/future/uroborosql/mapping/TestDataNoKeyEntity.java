package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Optional;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class TestDataNoKeyEntity {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	private Optional<String> memo;

	public TestDataNoKeyEntity() {
	}

	public TestDataNoKeyEntity(final long id, final String name, final int age, final LocalDate birthday,
			final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
	}

	/**
	 * id を取得します。
	 *
	 * @return id
	 */
	public long getId() {
		return id;
	}

	/**
	 * id を設定します。
	 *
	 * @param id id
	 */
	public void setId(final long id) {
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

	/**
	 * age を取得します。
	 *
	 * @return age
	 */
	public int getAge() {
		return age;
	}

	/**
	 * age を設定します。
	 *
	 * @param age age
	 */
	public void setAge(final int age) {
		this.age = age;
	}

	/**
	 * birthday を取得します。
	 *
	 * @return birthday
	 */
	public LocalDate getBirthday() {
		return birthday;
	}

	/**
	 * birthday を設定します。
	 *
	 * @param birthday birthday
	 */
	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	/**
	 * memo を取得します。
	 *
	 * @return memo
	 */
	public Optional<String> getMemo() {
		return memo;
	}

	/**
	 * memo を設定します。
	 *
	 * @param memo memo
	 */
	public void setMemo(final Optional<String> memo) {
		this.memo = memo;
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
