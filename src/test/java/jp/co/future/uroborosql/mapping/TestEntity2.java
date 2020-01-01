package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;

import jp.co.future.uroborosql.mapping.annotations.Table;

@Table(name = "TEST")
public class TestEntity2 {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	private int lockVersion = 0;

	public TestEntity2() {
	}

	public TestEntity2(final long id, final String name, final int age, final LocalDate birthday) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + age;
		result = prime * result + (birthday == null ? 0 : birthday.hashCode());
		result = prime * result + (int) (id ^ id >>> 32);
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
		TestEntity2 other = (TestEntity2) obj;
		if (age != other.age) {
			return false;
		}
		if (birthday == null) {
			if (other.birthday != null) {
				return false;
			}
		} else if (!birthday.equals(other.birthday)) {
			return false;
		}
		if (id != other.id) {
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
		return "TestEntity2 [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", lockVersion="
				+ lockVersion + "]";
	}

}
