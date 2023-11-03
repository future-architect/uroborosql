package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.util.Optional;

import org.junit.Test;

import jp.co.future.uroborosql.mapping.annotations.Table;

public class MappingColumnTest {

	@Test
	public void testMappingColumnType() throws Exception {
		// string
		MappingColumn col = MappingUtils.getMappingColumn(TestColType.class, "colStr");
		assertThat(col.getJavaType().getRawType(), sameInstance(String.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(true));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colChar");
		assertThat(col.getJavaType().getRawType(), sameInstance(char.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(true));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colCharacter");
		assertThat(col.getJavaType().getRawType(), sameInstance(Character.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(true));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		// number
		col = MappingUtils.getMappingColumn(TestColType.class, "colShort");
		assertThat(col.getJavaType().getRawType(), sameInstance(short.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colShortType");
		assertThat(col.getJavaType().getRawType(), sameInstance(Short.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colInt");
		assertThat(col.getJavaType().getRawType(), sameInstance(int.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colIntType");
		assertThat(col.getJavaType().getRawType(), sameInstance(Integer.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLong");
		assertThat(col.getJavaType().getRawType(), sameInstance(long.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLongType");
		assertThat(col.getJavaType().getRawType(), sameInstance(Long.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colFloat");
		assertThat(col.getJavaType().getRawType(), sameInstance(float.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colFloatType");
		assertThat(col.getJavaType().getRawType(), sameInstance(Float.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colDouble");
		assertThat(col.getJavaType().getRawType(), sameInstance(double.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colDoubleType");
		assertThat(col.getJavaType().getRawType(), sameInstance(Double.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigInteger");
		assertThat(col.getJavaType().getRawType(), sameInstance(BigInteger.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigDecimal");
		assertThat(col.getJavaType().getRawType(), sameInstance(BigDecimal.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		// temporal
		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDate");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalDate.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDateTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalDateTime.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetDateTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(OffsetDateTime.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colZonedDateTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(ZonedDateTime.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalTime.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(OffsetTime.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYear");
		assertThat(col.getJavaType().getRawType(), sameInstance(Year.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYearMonth");
		assertThat(col.getJavaType().getRawType(), sameInstance(YearMonth.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		// lagacy datetime
		col = MappingUtils.getMappingColumn(TestColType.class, "colUtilDate");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.util.Date.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlDate");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Date.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTime");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Time.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTimestamp");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Timestamp.class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

	}

	@Test
	public void testMappingColumnTypeOptional() throws Exception {
		// string
		MappingColumn col = MappingUtils.getMappingColumn(TestColType.class, "colStrOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(String.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(true));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colCharacterOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Character.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(true));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		// number
		col = MappingUtils.getMappingColumn(TestColType.class, "colShortTypeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Short.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colIntTypeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Integer.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLongTypeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Long.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colFloatTypeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Float.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colDoubleTypeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Double.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigIntegerOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(BigInteger.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigDecimalOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(BigDecimal.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(true));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		// temporal
		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDateOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(LocalDate.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDateTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(LocalDateTime.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetDateTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(OffsetDateTime.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colZonedDateTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(ZonedDateTime.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(LocalTime.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(OffsetTime.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYearOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(Year.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYearMonthOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(YearMonth.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(true));

		// lagacy datetime
		col = MappingUtils.getMappingColumn(TestColType.class, "colUtilDateOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(java.util.Date.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlDateOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(java.sql.Date.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTimeOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(java.sql.Time.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTimestampOpt");
		assertThat(col.getJavaType().getRawType(), sameInstance(Optional.class));
		assertThat(col.getJavaType().getParam(0).getRawType(), sameInstance(java.sql.Timestamp.class));
		assertThat(col.isOptional(), is(true));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(false));
		assertThat(col.isTemporal(), is(false));

	}

	@Test
	public void testMappingColumnTypeArray() throws Exception {
		// string
		MappingColumn col = MappingUtils.getMappingColumn(TestColType.class, "colStrArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(String[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colCharArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(char[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colCharacterArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Character[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		// number
		col = MappingUtils.getMappingColumn(TestColType.class, "colShortArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(short[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colShortTypeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Short[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colIntArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(int[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colIntTypeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Integer[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLongArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(long[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLongTypeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Long[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colFloatArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(float[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colFloatTypeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Float[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colDoubleArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(double[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colDoubleTypeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Double[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigIntegerArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(BigInteger[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colBigDecimalArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(BigDecimal[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		// temporal
		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDateArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalDate[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalDateTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalDateTime[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetDateTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(OffsetDateTime[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colZonedDateTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(ZonedDateTime[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colLocalTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(LocalTime[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colOffsetTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(OffsetTime[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYearArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(Year[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colYearMonthArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(YearMonth[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		// lagacy datetime
		col = MappingUtils.getMappingColumn(TestColType.class, "colUtilDateArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.util.Date[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlDateArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Date[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTimeArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Time[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

		col = MappingUtils.getMappingColumn(TestColType.class, "colSqlTimestampArr");
		assertThat(col.getJavaType().getRawType(), sameInstance(java.sql.Timestamp[].class));
		assertThat(col.isOptional(), is(false));
		assertThat(col.isString(), is(false));
		assertThat(col.isNumber(), is(false));
		assertThat(col.isArray(), is(true));
		assertThat(col.isTemporal(), is(false));

	}

	@Test
	public void testClearCache() throws Exception {
		MappingUtils.clearCache();
	}

	@Table(name = "TEST")
	public static class TestColType {
		private String colStr;
		private char colChar;
		private Character colCharacter;
		private short colShort;
		private Short colShortType;
		private int colInt;
		private Integer colIntType;
		private long colLong;
		private Long colLongType;
		private float colFloat;
		private Float colFloatType;
		private double colDouble;
		private Double colDoubleType;
		private BigInteger colBigInteger;
		private BigDecimal colBigDecimal;
		private LocalDate colLocalDate;
		private LocalDateTime colLocalDateTime;
		private OffsetDateTime colOffsetDateTime;
		private ZonedDateTime colZonedDateTime;
		private LocalTime colLocalTime;
		private OffsetTime colOffsetTime;
		private Year colYear;
		private YearMonth colYearMonth;
		private java.util.Date colUtilDate;
		private java.sql.Date colSqlDate;
		private java.sql.Time colSqlTime;
		private java.sql.Timestamp colSqlTimestamp;

		// Optional
		private Optional<String> colStrOpt;
		private Optional<Character> colCharacterOpt;
		private Optional<Short> colShortTypeOpt;
		private Optional<Integer> colIntTypeOpt;
		private Optional<Long> colLongTypeOpt;
		private Optional<Float> colFloatTypeOpt;
		private Optional<Double> colDoubleTypeOpt;
		private Optional<BigInteger> colBigIntegerOpt;
		private Optional<BigDecimal> colBigDecimalOpt;
		private Optional<LocalDate> colLocalDateOpt;
		private Optional<LocalDateTime> colLocalDateTimeOpt;
		private Optional<OffsetDateTime> colOffsetDateTimeOpt;
		private Optional<ZonedDateTime> colZonedDateTimeOpt;
		private Optional<LocalTime> colLocalTimeOpt;
		private Optional<OffsetTime> colOffsetTimeOpt;
		private Optional<Year> colYearOpt;
		private Optional<YearMonth> colYearMonthOpt;
		private Optional<java.util.Date> colUtilDateOpt;
		private Optional<java.sql.Date> colSqlDateOpt;
		private Optional<java.sql.Time> colSqlTimeOpt;
		private Optional<java.sql.Timestamp> colSqlTimestampOpt;

		// Array
		private String[] colStrArr;
		private char[] colCharArr;
		private Character[] colCharacterArr;
		private short[] colShortArr;
		private Short[] colShortTypeArr;
		private int[] colIntArr;
		private Integer[] colIntTypeArr;
		private long[] colLongArr;
		private Long[] colLongTypeArr;
		private float[] colFloatArr;
		private Float[] colFloatTypeArr;
		private double[] colDoubleArr;
		private Double[] colDoubleTypeArr;
		private BigInteger[] colBigIntegerArr;
		private BigDecimal[] colBigDecimalArr;
		private LocalDate[] colLocalDateArr;
		private LocalDateTime[] colLocalDateTimeArr;
		private OffsetDateTime[] colOffsetDateTimeArr;
		private ZonedDateTime[] colZonedDateTimeArr;
		private LocalTime[] colLocalTimeArr;
		private OffsetTime[] colOffsetTimeArr;
		private Year[] colYearArr;
		private YearMonth[] colYearMonthArr;
		private java.util.Date[] colUtilDateArr;
		private java.sql.Date[] colSqlDateArr;
		private java.sql.Time[] colSqlTimeArr;
		private java.sql.Timestamp[] colSqlTimestampArr;

		public TestColType() {
		}

		public String getColStr() {
			return colStr;
		}

		public void setColStr(final String colStr) {
			this.colStr = colStr;
		}

		public char getColChar() {
			return colChar;
		}

		public void setColChar(final char colChar) {
			this.colChar = colChar;
		}

		public Character getColCharacter() {
			return colCharacter;
		}

		public void setColCharacter(final Character colCharacter) {
			this.colCharacter = colCharacter;
		}

		public short getColShort() {
			return colShort;
		}

		public void setColShort(final short colShort) {
			this.colShort = colShort;
		}

		public Short getColShortType() {
			return colShortType;
		}

		public void setColShortType(final Short colShortType) {
			this.colShortType = colShortType;
		}

		public int getColInt() {
			return colInt;
		}

		public void setColInt(final int colInt) {
			this.colInt = colInt;
		}

		public Integer getColIntType() {
			return colIntType;
		}

		public void setColIntType(final Integer colIntType) {
			this.colIntType = colIntType;
		}

		public long getColLong() {
			return colLong;
		}

		public void setColLong(final long colLong) {
			this.colLong = colLong;
		}

		public Long getColLongType() {
			return colLongType;
		}

		public void setColLongType(final Long colLongType) {
			this.colLongType = colLongType;
		}

		public float getColFloat() {
			return colFloat;
		}

		public void setColFloat(final float colFloat) {
			this.colFloat = colFloat;
		}

		public Float getColFloatType() {
			return colFloatType;
		}

		public void setColFloatType(final Float colFloatType) {
			this.colFloatType = colFloatType;
		}

		public double getColDouble() {
			return colDouble;
		}

		public void setColDouble(final double colDouble) {
			this.colDouble = colDouble;
		}

		public Double getColDoubleType() {
			return colDoubleType;
		}

		public void setColDoubleType(final Double colDoubleType) {
			this.colDoubleType = colDoubleType;
		}

		public BigInteger getColBigInteger() {
			return colBigInteger;
		}

		public void setColBigInteger(final BigInteger colBigInteger) {
			this.colBigInteger = colBigInteger;
		}

		public BigDecimal getColBigDecimal() {
			return colBigDecimal;
		}

		public void setColBigDecimal(final BigDecimal colBigDecimal) {
			this.colBigDecimal = colBigDecimal;
		}

		public LocalDate getColLocalDate() {
			return colLocalDate;
		}

		public void setColLocalDate(final LocalDate colLocalDate) {
			this.colLocalDate = colLocalDate;
		}

		public LocalDateTime getColLocalDateTime() {
			return colLocalDateTime;
		}

		public void setColLocalDateTime(final LocalDateTime colLocalDateTime) {
			this.colLocalDateTime = colLocalDateTime;
		}

		public OffsetDateTime getColOffsetDateTime() {
			return colOffsetDateTime;
		}

		public void setColOffsetDateTime(final OffsetDateTime colOffsetDateTime) {
			this.colOffsetDateTime = colOffsetDateTime;
		}

		public ZonedDateTime getColZonedDateTime() {
			return colZonedDateTime;
		}

		public void setColZonedDateTime(final ZonedDateTime colZonedDateTime) {
			this.colZonedDateTime = colZonedDateTime;
		}

		public LocalTime getColLocalTime() {
			return colLocalTime;
		}

		public void setColLocalTime(final LocalTime colLocalTime) {
			this.colLocalTime = colLocalTime;
		}

		public OffsetTime getColOffsetTime() {
			return colOffsetTime;
		}

		public void setColOffsetTime(final OffsetTime colOffsetTime) {
			this.colOffsetTime = colOffsetTime;
		}

		public Year getColYear() {
			return colYear;
		}

		public void setColYear(final Year colYear) {
			this.colYear = colYear;
		}

		public YearMonth getColYearMonth() {
			return colYearMonth;
		}

		public void setColYearMonth(final YearMonth colYearMonth) {
			this.colYearMonth = colYearMonth;
		}

		public java.util.Date getColUtilDate() {
			return colUtilDate;
		}

		public void setColUtilDate(final java.util.Date colUtilDate) {
			this.colUtilDate = colUtilDate;
		}

		public java.sql.Date getColSqlDate() {
			return colSqlDate;
		}

		public void setColSqlDate(final java.sql.Date colSqlDate) {
			this.colSqlDate = colSqlDate;
		}

		public java.sql.Time getColSqlTime() {
			return colSqlTime;
		}

		public void setColSqlTime(final java.sql.Time colSqlTime) {
			this.colSqlTime = colSqlTime;
		}

		public java.sql.Timestamp getColSqlTimestamp() {
			return colSqlTimestamp;
		}

		public void setColSqlTimestamp(final java.sql.Timestamp colSqlTimestamp) {
			this.colSqlTimestamp = colSqlTimestamp;
		}

		public Optional<String> getColStrOpt() {
			return colStrOpt;
		}

		public void setColStrOpt(final Optional<String> colStrOpt) {
			this.colStrOpt = colStrOpt;
		}

		public Optional<Character> getColCharacterOpt() {
			return colCharacterOpt;
		}

		public void setColCharacterOpt(final Optional<Character> colCharacterOpt) {
			this.colCharacterOpt = colCharacterOpt;
		}

		public Optional<Short> getColShortTypeOpt() {
			return colShortTypeOpt;
		}

		public void setColShortTypeOpt(final Optional<Short> colShortTypeOpt) {
			this.colShortTypeOpt = colShortTypeOpt;
		}

		public Optional<Integer> getColIntTypeOpt() {
			return colIntTypeOpt;
		}

		public void setColIntTypeOpt(final Optional<Integer> colIntTypeOpt) {
			this.colIntTypeOpt = colIntTypeOpt;
		}

		public Optional<Long> getColLongTypeOpt() {
			return colLongTypeOpt;
		}

		public void setColLongTypeOpt(final Optional<Long> colLongTypeOpt) {
			this.colLongTypeOpt = colLongTypeOpt;
		}

		public Optional<Float> getColFloatTypeOpt() {
			return colFloatTypeOpt;
		}

		public void setColFloatTypeOpt(final Optional<Float> colFloatTypeOpt) {
			this.colFloatTypeOpt = colFloatTypeOpt;
		}

		public Optional<Double> getColDoubleTypeOpt() {
			return colDoubleTypeOpt;
		}

		public void setColDoubleTypeOpt(final Optional<Double> colDoubleTypeOpt) {
			this.colDoubleTypeOpt = colDoubleTypeOpt;
		}

		public Optional<BigInteger> getColBigIntegerOpt() {
			return colBigIntegerOpt;
		}

		public void setColBigIntegerOpt(final Optional<BigInteger> colBigIntegerOpt) {
			this.colBigIntegerOpt = colBigIntegerOpt;
		}

		public Optional<BigDecimal> getColBigDecimalOpt() {
			return colBigDecimalOpt;
		}

		public void setColBigDecimalOpt(final Optional<BigDecimal> colBigDecimalOpt) {
			this.colBigDecimalOpt = colBigDecimalOpt;
		}

		public Optional<LocalDate> getColLocalDateOpt() {
			return colLocalDateOpt;
		}

		public void setColLocalDateOpt(final Optional<LocalDate> colLocalDateOpt) {
			this.colLocalDateOpt = colLocalDateOpt;
		}

		public Optional<LocalDateTime> getColLocalDateTimeOpt() {
			return colLocalDateTimeOpt;
		}

		public void setColLocalDateTimeOpt(final Optional<LocalDateTime> colLocalDateTimeOpt) {
			this.colLocalDateTimeOpt = colLocalDateTimeOpt;
		}

		public Optional<OffsetDateTime> getColOffsetDateTimeOpt() {
			return colOffsetDateTimeOpt;
		}

		public void setColOffsetDateTimeOpt(final Optional<OffsetDateTime> colOffsetDateTimeOpt) {
			this.colOffsetDateTimeOpt = colOffsetDateTimeOpt;
		}

		public Optional<ZonedDateTime> getColZonedDateTimeOpt() {
			return colZonedDateTimeOpt;
		}

		public void setColZonedDateTimeOpt(final Optional<ZonedDateTime> colZonedDateTimeOpt) {
			this.colZonedDateTimeOpt = colZonedDateTimeOpt;
		}

		public Optional<LocalTime> getColLocalTimeOpt() {
			return colLocalTimeOpt;
		}

		public void setColLocalTimeOpt(final Optional<LocalTime> colLocalTimeOpt) {
			this.colLocalTimeOpt = colLocalTimeOpt;
		}

		public Optional<OffsetTime> getColOffsetTimeOpt() {
			return colOffsetTimeOpt;
		}

		public void setColOffsetTimeOpt(final Optional<OffsetTime> colOffsetTimeOpt) {
			this.colOffsetTimeOpt = colOffsetTimeOpt;
		}

		public Optional<Year> getColYearOpt() {
			return colYearOpt;
		}

		public void setColYearOpt(final Optional<Year> colYearOpt) {
			this.colYearOpt = colYearOpt;
		}

		public Optional<YearMonth> getColYearMonthOpt() {
			return colYearMonthOpt;
		}

		public void setColYearMonthOpt(final Optional<YearMonth> colYearMonthOpt) {
			this.colYearMonthOpt = colYearMonthOpt;
		}

		public Optional<java.util.Date> getColUtilDateOpt() {
			return colUtilDateOpt;
		}

		public void setColUtilDateOpt(final Optional<java.util.Date> colUtilDateOpt) {
			this.colUtilDateOpt = colUtilDateOpt;
		}

		public Optional<java.sql.Date> getColSqlDateOpt() {
			return colSqlDateOpt;
		}

		public void setColSqlDateOpt(final Optional<java.sql.Date> colSqlDateOpt) {
			this.colSqlDateOpt = colSqlDateOpt;
		}

		public Optional<java.sql.Time> getColSqlTimeOpt() {
			return colSqlTimeOpt;
		}

		public void setColSqlTimeOpt(final Optional<java.sql.Time> colSqlTimeOpt) {
			this.colSqlTimeOpt = colSqlTimeOpt;
		}

		public Optional<java.sql.Timestamp> getColSqlTimestampOpt() {
			return colSqlTimestampOpt;
		}

		public void setColSqlTimestampOpt(final Optional<java.sql.Timestamp> colSqlTimestampOpt) {
			this.colSqlTimestampOpt = colSqlTimestampOpt;
		}

		public String[] getColStrArr() {
			return colStrArr;
		}

		public void setColStrArr(final String[] colStrArr) {
			this.colStrArr = colStrArr;
		}

		public char[] getColCharArr() {
			return colCharArr;
		}

		public void setColCharArr(final char[] colCharArr) {
			this.colCharArr = colCharArr;
		}

		public Character[] getColCharacterArr() {
			return colCharacterArr;
		}

		public void setColCharacterArr(final Character[] colCharacterArr) {
			this.colCharacterArr = colCharacterArr;
		}

		public short[] getColShortArr() {
			return colShortArr;
		}

		public void setColShortArr(final short[] colShortArr) {
			this.colShortArr = colShortArr;
		}

		public Short[] getColShortTypeArr() {
			return colShortTypeArr;
		}

		public void setColShortTypeArr(final Short[] colShortTypeArr) {
			this.colShortTypeArr = colShortTypeArr;
		}

		public int[] getColIntArr() {
			return colIntArr;
		}

		public void setColIntArr(final int[] colIntArr) {
			this.colIntArr = colIntArr;
		}

		public Integer[] getColIntTypeArr() {
			return colIntTypeArr;
		}

		public void setColIntTypeArr(final Integer[] colIntTypeArr) {
			this.colIntTypeArr = colIntTypeArr;
		}

		public long[] getColLongArr() {
			return colLongArr;
		}

		public void setColLongArr(final long[] colLongArr) {
			this.colLongArr = colLongArr;
		}

		public Long[] getColLongTypeArr() {
			return colLongTypeArr;
		}

		public void setColLongTypeArr(final Long[] colLongTypeArr) {
			this.colLongTypeArr = colLongTypeArr;
		}

		public float[] getColFloatArr() {
			return colFloatArr;
		}

		public void setColFloatArr(final float[] colFloatArr) {
			this.colFloatArr = colFloatArr;
		}

		public Float[] getColFloatTypeArr() {
			return colFloatTypeArr;
		}

		public void setColFloatTypeArr(final Float[] colFloatTypeArr) {
			this.colFloatTypeArr = colFloatTypeArr;
		}

		public double[] getColDoubleArr() {
			return colDoubleArr;
		}

		public void setColDoubleArr(final double[] colDoubleArr) {
			this.colDoubleArr = colDoubleArr;
		}

		public Double[] getColDoubleTypeArr() {
			return colDoubleTypeArr;
		}

		public void setColDoubleTypeArr(final Double[] colDoubleTypeArr) {
			this.colDoubleTypeArr = colDoubleTypeArr;
		}

		public BigInteger[] getColBigIntegerArr() {
			return colBigIntegerArr;
		}

		public void setColBigIntegerArr(final BigInteger[] colBigIntegerArr) {
			this.colBigIntegerArr = colBigIntegerArr;
		}

		public BigDecimal[] getColBigDecimalArr() {
			return colBigDecimalArr;
		}

		public void setColBigDecimalArr(final BigDecimal[] colBigDecimalArr) {
			this.colBigDecimalArr = colBigDecimalArr;
		}

		public LocalDate[] getColLocalDateArr() {
			return colLocalDateArr;
		}

		public void setColLocalDateArr(final LocalDate[] colLocalDateArr) {
			this.colLocalDateArr = colLocalDateArr;
		}

		public LocalDateTime[] getColLocalDateTimeArr() {
			return colLocalDateTimeArr;
		}

		public void setColLocalDateTimeArr(final LocalDateTime[] colLocalDateTimeArr) {
			this.colLocalDateTimeArr = colLocalDateTimeArr;
		}

		public OffsetDateTime[] getColOffsetDateTimeArr() {
			return colOffsetDateTimeArr;
		}

		public void setColOffsetDateTimeArr(final OffsetDateTime[] colOffsetDateTimeArr) {
			this.colOffsetDateTimeArr = colOffsetDateTimeArr;
		}

		public ZonedDateTime[] getColZonedDateTimeArr() {
			return colZonedDateTimeArr;
		}

		public void setColZonedDateTimeArr(final ZonedDateTime[] colZonedDateTimeArr) {
			this.colZonedDateTimeArr = colZonedDateTimeArr;
		}

		public LocalTime[] getColLocalTimeArr() {
			return colLocalTimeArr;
		}

		public void setColLocalTimeArr(final LocalTime[] colLocalTimeArr) {
			this.colLocalTimeArr = colLocalTimeArr;
		}

		public OffsetTime[] getColOffsetTimeArr() {
			return colOffsetTimeArr;
		}

		public void setColOffsetTimeArr(final OffsetTime[] colOffsetTimeArr) {
			this.colOffsetTimeArr = colOffsetTimeArr;
		}

		public Year[] getColYearArr() {
			return colYearArr;
		}

		public void setColYearArr(final Year[] colYearArr) {
			this.colYearArr = colYearArr;
		}

		public YearMonth[] getColYearMonthArr() {
			return colYearMonthArr;
		}

		public void setColYearMonthArr(final YearMonth[] colYearMonthArr) {
			this.colYearMonthArr = colYearMonthArr;
		}

		public java.util.Date[] getColUtilDateArr() {
			return colUtilDateArr;
		}

		public void setColUtilDateArr(final java.util.Date[] colUtilDateArr) {
			this.colUtilDateArr = colUtilDateArr;
		}

		public java.sql.Date[] getColSqlDateArr() {
			return colSqlDateArr;
		}

		public void setColSqlDateArr(final java.sql.Date[] colSqlDateArr) {
			this.colSqlDateArr = colSqlDateArr;
		}

		public java.sql.Time[] getColSqlTimeArr() {
			return colSqlTimeArr;
		}

		public void setColSqlTimeArr(final java.sql.Time[] colSqlTimeArr) {
			this.colSqlTimeArr = colSqlTimeArr;
		}

		public java.sql.Timestamp[] getColSqlTimestampArr() {
			return colSqlTimestampArr;
		}

		public void setColSqlTimestampArr(final java.sql.Timestamp[] colSqlTimestampArr) {
			this.colSqlTimestampArr = colSqlTimestampArr;
		}

	}
}
