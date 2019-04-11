package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST")
public class TestEntity3 {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	@Version
	private int lockVersion = 0;

	public TestEntity3() {
	}

	public TestEntity3(final long id, final String name, final int age, final LocalDate birthday) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
	}

	public interface Names {
		String Id = "id";
		String Name = "name";
		String Age = "age";
		String Birthday = "birthday";
	}

	public interface Cols {
		String Id = "id";
		String Name = "name";
		String Age = "age";
		String Birthday = "birthday";
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

	public int getLockVersion() {
		return lockVersion;
	}

	public void setLockVersion(final int lockVersion) {
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
