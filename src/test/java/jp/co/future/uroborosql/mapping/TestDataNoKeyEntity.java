package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Optional;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + age;
		result = prime * result + (birthday == null ? 0 : birthday.hashCode());
		result = prime * result + (int) (id ^ id >>> 32);
		result = prime * result + (memo == null ? 0 : memo.hashCode());
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
		TestDataNoKeyEntity other = (TestDataNoKeyEntity) obj;
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
		if (memo == null) {
			if (other.memo != null) {
				return false;
			}
		} else if (!memo.equals(other.memo)) {
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
		return "TestDataNoKeyEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday
				+ ", memo=" + memo + "]";
	}

}
