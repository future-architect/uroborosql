package jp.co.future.uroborosql.mapping.mapper;

import static jp.co.future.uroborosql.mapping.mapper.Helper.newProxy;
import static jp.co.future.uroborosql.mapping.mapper.Helper.newResultSet;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;

import jp.co.future.uroborosql.mapping.JavaType;

import org.junit.Test;

public class PropertyMapperManagerTest {

	@Test
	public void test() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager();
		assertThat(mapper.getValue(JavaType.of(String.class), newResultSet("getString", "value"), 1), is("value"));
		assertThat(mapper.getValue(JavaType.of(String.class), newResultSet("getString", null), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(boolean.class), newResultSet("getBoolean", true), 1), is(true));
		assertThat(mapper.getValue(JavaType.of(boolean.class), newResultSet("getBoolean", false, "wasNull", false), 1), is(false));
		assertThat(mapper.getValue(JavaType.of(boolean.class), newResultSet("getBoolean", false, "wasNull", true), 1), is(false));

		assertThat(mapper.getValue(JavaType.of(Boolean.class), newResultSet("getBoolean", true), 1), is(true));
		assertThat(mapper.getValue(JavaType.of(Boolean.class), newResultSet("getBoolean", false, "wasNull", false), 1), is(false));
		assertThat(mapper.getValue(JavaType.of(Boolean.class), newResultSet("getBoolean", false, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(byte.class), newResultSet("getByte", (byte) 1), 1), is((byte) 1));
		assertThat(mapper.getValue(JavaType.of(byte.class), newResultSet("getByte", (byte) 0, "wasNull", false), 1), is((byte) 0));
		assertThat(mapper.getValue(JavaType.of(byte.class), newResultSet("getByte", (byte) 0, "wasNull", true), 1), is((byte) 0));

		assertThat(mapper.getValue(JavaType.of(Byte.class), newResultSet("getByte", (byte) 1), 1), is((byte) 1));
		assertThat(mapper.getValue(JavaType.of(Byte.class), newResultSet("getByte", (byte) 0, "wasNull", false), 1), is((byte) 0));
		assertThat(mapper.getValue(JavaType.of(Byte.class), newResultSet("getByte", (byte) 0, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(short.class), newResultSet("getShort", (short) 1), 1), is((short) 1));
		assertThat(mapper.getValue(JavaType.of(short.class), newResultSet("getShort", (short) 0, "wasNull", false), 1), is((short) 0));
		assertThat(mapper.getValue(JavaType.of(short.class), newResultSet("getShort", (short) 0, "wasNull", true), 1), is((short) 0));

		assertThat(mapper.getValue(JavaType.of(Short.class), newResultSet("getShort", (short) 1), 1), is((short) 1));
		assertThat(mapper.getValue(JavaType.of(Short.class), newResultSet("getShort", (short) 0, "wasNull", false), 1), is((short) 0));
		assertThat(mapper.getValue(JavaType.of(Short.class), newResultSet("getShort", (short) 0, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(int.class), newResultSet("getInt", 1), 1), is(1));
		assertThat(mapper.getValue(JavaType.of(int.class), newResultSet("getInt", 0, "wasNull", false), 1), is(0));
		assertThat(mapper.getValue(JavaType.of(int.class), newResultSet("getInt", 0, "wasNull", true), 1), is(0));

		assertThat(mapper.getValue(JavaType.of(Integer.class), newResultSet("getInt", 1), 1), is(1));
		assertThat(mapper.getValue(JavaType.of(Integer.class), newResultSet("getInt", 0, "wasNull", false), 1), is(0));
		assertThat(mapper.getValue(JavaType.of(Integer.class), newResultSet("getInt", 0, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(long.class), newResultSet("getLong", 1L), 1), is(1L));
		assertThat(mapper.getValue(JavaType.of(long.class), newResultSet("getLong", 0L, "wasNull", false), 1), is(0L));
		assertThat(mapper.getValue(JavaType.of(long.class), newResultSet("getLong", 0L, "wasNull", true), 1), is(0L));

		assertThat(mapper.getValue(JavaType.of(Long.class), newResultSet("getLong", 1L), 1), is(1L));
		assertThat(mapper.getValue(JavaType.of(Long.class), newResultSet("getLong", 0L, "wasNull", false), 1), is(0L));
		assertThat(mapper.getValue(JavaType.of(Long.class), newResultSet("getLong", 0L, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(float.class), newResultSet("getFloat", 1.2F), 1), is(1.2F));
		assertThat(mapper.getValue(JavaType.of(float.class), newResultSet("getFloat", 0F, "wasNull", false), 1), is(0F));
		assertThat(mapper.getValue(JavaType.of(float.class), newResultSet("getFloat", 0F, "wasNull", true), 1), is(0F));

		assertThat(mapper.getValue(JavaType.of(Float.class), newResultSet("getFloat", 1.2F), 1), is(1.2F));
		assertThat(mapper.getValue(JavaType.of(Float.class), newResultSet("getFloat", 0F, "wasNull", false), 1), is(0F));
		assertThat(mapper.getValue(JavaType.of(Float.class), newResultSet("getFloat", 0F, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(double.class), newResultSet("getDouble", 1.2D), 1), is(1.2D));
		assertThat(mapper.getValue(JavaType.of(double.class), newResultSet("getDouble", 0D, "wasNull", false), 1), is(0D));
		assertThat(mapper.getValue(JavaType.of(double.class), newResultSet("getDouble", 0D, "wasNull", true), 1), is(0D));

		assertThat(mapper.getValue(JavaType.of(Double.class), newResultSet("getDouble", 1.2D), 1), is(1.2D));
		assertThat(mapper.getValue(JavaType.of(Double.class), newResultSet("getDouble", 0D, "wasNull", false), 1), is(0D));
		assertThat(mapper.getValue(JavaType.of(Double.class), newResultSet("getDouble", 0D, "wasNull", true), 1), nullValue());

		assertThat(mapper.getValue(JavaType.of(BigDecimal.class), newResultSet("getBigDecimal", BigDecimal.valueOf(123.123)), 1),
				is(BigDecimal.valueOf(123.123)));
		assertThat(mapper.getValue(JavaType.of(byte[].class), newResultSet("getBytes", "abc".getBytes(StandardCharsets.UTF_8)), 1),
				is("abc".getBytes(StandardCharsets.UTF_8)));

		java.sql.Timestamp timestamp = java.sql.Timestamp.valueOf(LocalDateTime.now());
		assertThat(mapper.getValue(JavaType.of(java.sql.Timestamp.class), newResultSet("getTimestamp", timestamp), 1), is(timestamp));

		java.sql.Time time = java.sql.Time.valueOf(LocalTime.now());
		assertThat(mapper.getValue(JavaType.of(java.sql.Time.class), newResultSet("getTime", time), 1), is(time));

		java.sql.Date date = java.sql.Date.valueOf(LocalDate.now());
		assertThat(mapper.getValue(JavaType.of(java.sql.Date.class), newResultSet("getDate", date), 1), is(date));
		assertThat(mapper.getValue(JavaType.of(Date.class), newResultSet("getTimestamp", timestamp), 1), is(timestamp));

		java.sql.Array array = newProxy(java.sql.Array.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.Array.class), newResultSet("getArray", array), 1), is(array));

		java.sql.Ref ref = newProxy(java.sql.Ref.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.Ref.class), newResultSet("getRef", ref), 1), is(ref));

		java.sql.Blob blob = newProxy(java.sql.Blob.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.Blob.class), newResultSet("getBlob", blob), 1), is(blob));

		java.sql.Clob clob = newProxy(java.sql.Clob.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.Clob.class), newResultSet("getClob", clob), 1), is(clob));

		java.sql.NClob nclob = newProxy(java.sql.NClob.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.NClob.class), newResultSet("getNClob", nclob), 1), is(nclob));

		java.sql.SQLXML sqlxml = newProxy(java.sql.SQLXML.class);
		assertThat(mapper.getValue(JavaType.of(java.sql.SQLXML.class), newResultSet("getSQLXML", sqlxml), 1), is(sqlxml));

		assertThat(mapper.getValue(JavaType.of(OptionalInt.class), newResultSet("getInt", 1), 1), is(OptionalInt.of(1)));
		assertThat(mapper.getValue(JavaType.of(OptionalInt.class), newResultSet("getInt", 0, "wasNull", false), 1), is(OptionalInt.of(0)));
		assertThat(mapper.getValue(JavaType.of(OptionalInt.class), newResultSet("getInt", 0, "wasNull", true), 1), is(OptionalInt.empty()));

		assertThat(mapper.getValue(JavaType.of(OptionalLong.class), newResultSet("getLong", 1L), 1), is(OptionalLong.of(1L)));
		assertThat(mapper.getValue(JavaType.of(OptionalLong.class), newResultSet("getLong", 0L, "wasNull", false), 1), is(OptionalLong.of(0L)));
		assertThat(mapper.getValue(JavaType.of(OptionalLong.class), newResultSet("getLong", 0L, "wasNull", true), 1), is(OptionalLong.empty()));

		assertThat(mapper.getValue(JavaType.of(OptionalDouble.class), newResultSet("getDouble", 1.2D), 1), is(OptionalDouble.of(1.2D)));
		assertThat(mapper.getValue(JavaType.of(OptionalDouble.class), newResultSet("getDouble", 0D, "wasNull", false), 1), is(OptionalDouble.of(0D)));
		assertThat(mapper.getValue(JavaType.of(OptionalDouble.class), newResultSet("getDouble", 0D, "wasNull", true), 1), is(OptionalDouble.empty()));
	}

	@Test
	public void testCustom() throws NoSuchMethodException, SecurityException, SQLException {
		PropertyMapperManager mapper = new PropertyMapperManager();

		mapper.addMapper(new PropertyMapper<String>() {

			@Override
			public boolean canAccept(final Class<?> type) {
				return String.class.equals(type);
			}

			@Override
			public String getValue(final JavaType type, final ResultSet rs, final int columnIndex, final PropertyMapperManager mapperManager)
					throws SQLException {
				return rs.getString(columnIndex).toUpperCase();
			}
		});
		assertThat(mapper.getValue(JavaType.of(String.class), newResultSet("getString", "value"), 1), is("VALUE"));
		assertThat(mapper.getValue(JavaType.of(int.class), newResultSet("getInt", 1), 1), is(1));

	}

}
