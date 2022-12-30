package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Optional;

public class TestEntity {
	private Long id;
	private String name;
	private Integer age;
	private LocalDate birthday;
	private Optional<String> memo;
	private Integer lockVersion;

	public TestEntity() {
	}

	public TestEntity(final Long id, final String name, final Integer age, final LocalDate birthday,
			final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
		this.lockVersion = 0;
	}

	public Long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public Integer getAge() {
		return this.age;
	}

	public LocalDate getBirthday() {
		return this.birthday;
	}

	public Optional<String> getMemo() {
		return this.memo;
	}

	public Integer getLockVersion() {
		return this.lockVersion;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setAge(final Integer age) {
		this.age = age;
	}

	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	public void setMemo(final Optional<String> memo) {
		this.memo = memo;
	}

	public void setLockVersion(final Integer lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((age == null) ? 0 : age.hashCode());
		result = prime * result + ((birthday == null) ? 0 : birthday.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((lockVersion == null) ? 0 : lockVersion.hashCode());
		result = prime * result + ((memo == null) ? 0 : memo.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		TestEntity other = (TestEntity) obj;
		if (age == null) {
			if (other.age != null)
				return false;
		} else if (!age.equals(other.age))
			return false;
		if (birthday == null) {
			if (other.birthday != null)
				return false;
		} else if (!birthday.equals(other.birthday))
			return false;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (lockVersion == null) {
			if (other.lockVersion != null)
				return false;
		} else if (!lockVersion.equals(other.lockVersion))
			return false;
		if (memo == null) {
			if (other.memo != null)
				return false;
		} else if (!memo.equals(other.memo))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "TestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", memo=" + memo
				+ ", lockVersion=" + lockVersion + "]";
	}

}
