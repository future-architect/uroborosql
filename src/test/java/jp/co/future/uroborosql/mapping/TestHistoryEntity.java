package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;

public class TestHistoryEntity {
	private Long id;
	private LocalDate startAt;
	private LocalDate finishAt;
	private String name;

	public TestHistoryEntity() {
	}

	public TestHistoryEntity(final Long id, final LocalDate startAt, final LocalDate finishAt, final String name) {
		this.id = id;
		this.startAt = startAt;
		this.finishAt = finishAt;
		this.name = name;
	}

	public Long getId() {
		return id;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public LocalDate getStartAt() {
		return startAt;
	}

	public void setStartAt(final LocalDate startAt) {
		this.startAt = startAt;
	}

	public LocalDate getFinishAt() {
		return finishAt;
	}

	public void setFinishAt(final LocalDate finishAt) {
		this.finishAt = finishAt;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (finishAt == null ? 0 : finishAt.hashCode());
		result = prime * result + (int) (id ^ id >>> 32);
		result = prime * result + (name == null ? 0 : name.hashCode());
		result = prime * result + (startAt == null ? 0 : startAt.hashCode());
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
		TestHistoryEntity other = (TestHistoryEntity) obj;
		if (finishAt == null) {
			if (other.finishAt != null) {
				return false;
			}
		} else if (!finishAt.equals(other.finishAt)) {
			return false;
		}
		if (id != other.id) {
			return false;
		}
		if (name == null) {
			if (other.name != null) {
				return false;
			}
		} else if (!name.equals(other.name)) {
			return false;
		}
		if (startAt == null) {
			if (other.startAt != null) {
				return false;
			}
		} else if (!startAt.equals(other.startAt)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestHistoryEntity [id=" + id + ", startAt=" + startAt + ", finishAt=" + finishAt + ", name=" + name
				+ "]";
	}

}
