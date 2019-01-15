package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import jp.co.future.uroborosql.mapping.annotations.Table;

@Table(name = "TEST")
public class TestEntityForInserts {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	private String memo;

	public TestEntityForInserts() {
	}

	public TestEntityForInserts(final long id, final String name, final int age, final LocalDate birthday,
			final String memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
	}

	public long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public int getAge() {
		return this.age;
	}

	public LocalDate getBirthday() {
		return this.birthday;
	}

	public String getMemo() {
		return this.memo;
	}

	public void setId(final long id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setAge(final int age) {
		this.age = age;
	}

	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	public void setMemo(final String memo) {
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
