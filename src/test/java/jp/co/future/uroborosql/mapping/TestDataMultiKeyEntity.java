package jp.co.future.uroborosql.mapping;

public class TestDataMultiKeyEntity {
	private long id;
	private String key;
	private String name;

	public TestDataMultiKeyEntity() {
	}

	public TestDataMultiKeyEntity(final long id, final String key, final String name) {
		this.id = id;
		this.key = key;
		this.name = name;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ id >>> 32);
		result = prime * result + (key == null ? 0 : key.hashCode());
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
		TestDataMultiKeyEntity other = (TestDataMultiKeyEntity) obj;
		if (id != other.id) {
			return false;
		}
		if (key == null) {
			if (other.key != null) {
				return false;
			}
		} else if (!key.equals(other.key)) {
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
		return "TestDataMultiKeyEntity [id=" + id + ", key=" + key + ", name=" + name + "]";
	}

}
