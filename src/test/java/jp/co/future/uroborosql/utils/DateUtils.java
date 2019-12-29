package jp.co.future.uroborosql.utils;

import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public final class DateUtils {
	/**
	 * Number of milliseconds in a standard second.
	 * @since 2.1
	 */
	public static final long MILLIS_PER_SECOND = 1000;
	/**
	 * Number of milliseconds in a standard minute.
	 * @since 2.1
	 */
	public static final long MILLIS_PER_MINUTE = 60 * MILLIS_PER_SECOND;
	/**
	 * Number of milliseconds in a standard hour.
	 * @since 2.1
	 */
	public static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;
	/**
	 * Number of milliseconds in a standard day.
	 * @since 2.1
	 */
	public static final long MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR;

	/**
	 * This is half a month, so this represents whether a date is in the top
	 * or bottom half of the month.
	 */
	public static final int SEMI_MONTH = 1001;

	private static final int[][] fields = {
			{ Calendar.MILLISECOND },
			{ Calendar.SECOND },
			{ Calendar.MINUTE },
			{ Calendar.HOUR_OF_DAY, Calendar.HOUR },
			{ Calendar.DATE, Calendar.DAY_OF_MONTH, Calendar.AM_PM
			/* Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK, Calendar.DAY_OF_WEEK_IN_MONTH */
			},
			{ Calendar.MONTH, DateUtils.SEMI_MONTH },
			{ Calendar.YEAR },
			{ Calendar.ERA } };

	/**
	 * Calendar modification types.
	 */
	private enum ModifyType {
		/**
		 * Truncation.
		 */
		TRUNCATE,

		/**
		 * Rounding.
		 */
		ROUND,

		/**
		 * Ceiling.
		 */
		CEILING
	}

	private DateUtils() {
	}

	//-----------------------------------------------------------------------
	/**
	 * <p>Parses a string representing a date by trying a variety of different parsers.</p>
	 *
	 * <p>The parse will try each parse pattern in turn.
	 * A parse is only deemed successful if it parses the whole of the input string.
	 * If no parse patterns match, a ParseException is thrown.</p>
	 * The parser will be lenient toward the parsed date.
	 *
	 * @param str  the date to parse, not null
	 * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
	 * @return the parsed date
	 * @throws IllegalArgumentException if the date string or pattern array is null
	 * @throws ParseException if none of the date patterns were suitable (or there were none)
	 */
	public static Date parseDate(final String str, final String... parsePatterns) throws ParseException {
		return parseDate(str, null, parsePatterns);
	}

	//-----------------------------------------------------------------------
	/**
	 * <p>Parses a string representing a date by trying a variety of different parsers,
	 * using the default date format symbols for the given locale.</p>
	 *
	 * <p>The parse will try each parse pattern in turn.
	 * A parse is only deemed successful if it parses the whole of the input string.
	 * If no parse patterns match, a ParseException is thrown.</p>
	 * The parser will be lenient toward the parsed date.
	 *
	 * @param str  the date to parse, not null
	 * @param locale the locale whose date format symbols should be used. If <code>null</code>,
	 * the system locale is used (as per {@link #parseDate(String, String...)}).
	 * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
	 * @return the parsed date
	 * @throws IllegalArgumentException if the date string or pattern array is null
	 * @throws ParseException if none of the date patterns were suitable (or there were none)
	 * @since 3.2
	 */
	public static Date parseDate(final String str, final Locale locale, final String... parsePatterns)
			throws ParseException {
		return parseDateWithLeniency(str, locale, parsePatterns, true);
	}

	//-----------------------------------------------------------------------
	/**
	 * <p>Parses a string representing a date by trying a variety of different parsers.</p>
	 *
	 * <p>The parse will try each parse pattern in turn.
	 * A parse is only deemed successful if it parses the whole of the input string.
	 * If no parse patterns match, a ParseException is thrown.</p>
	 * The parser parses strictly - it does not allow for dates such as "February 942, 1996".
	 *
	 * @param str  the date to parse, not null
	 * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
	 * @return the parsed date
	 * @throws IllegalArgumentException if the date string or pattern array is null
	 * @throws ParseException if none of the date patterns were suitable
	 * @since 2.5
	 */
	public static Date parseDateStrictly(final String str, final String... parsePatterns) throws ParseException {
		return parseDateStrictly(str, null, parsePatterns);
	}

	/**
	 * <p>Parses a string representing a date by trying a variety of different parsers,
	 * using the default date format symbols for the given locale..</p>
	 *
	 * <p>The parse will try each parse pattern in turn.
	 * A parse is only deemed successful if it parses the whole of the input string.
	 * If no parse patterns match, a ParseException is thrown.</p>
	 * The parser parses strictly - it does not allow for dates such as "February 942, 1996".
	 *
	 * @param str  the date to parse, not null
	 * @param locale the locale whose date format symbols should be used. If <code>null</code>,
	 * the system locale is used (as per {@link #parseDateStrictly(String, String...)}).
	 * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
	 * @return the parsed date
	 * @throws IllegalArgumentException if the date string or pattern array is null
	 * @throws ParseException if none of the date patterns were suitable
	 * @since 3.2
	 */
	public static Date parseDateStrictly(final String str, final Locale locale, final String... parsePatterns)
			throws ParseException {
		return parseDateWithLeniency(str, locale, parsePatterns, false);
	}

	/**
	 * <p>Parses a string representing a date by trying a variety of different parsers.</p>
	 *
	 * <p>The parse will try each parse pattern in turn.
	 * A parse is only deemed successful if it parses the whole of the input string.
	 * If no parse patterns match, a ParseException is thrown.</p>
	 *
	 * @param str  the date to parse, not null
	 * @param locale the locale to use when interpretting the pattern, can be null in which
	 * case the default system locale is used
	 * @param parsePatterns  the date format patterns to use, see SimpleDateFormat, not null
	 * @param lenient Specify whether or not date/time parsing is to be lenient.
	 * @return the parsed date
	 * @throws IllegalArgumentException if the date string or pattern array is null
	 * @throws ParseException if none of the date patterns were suitable
	 * @see java.util.Calendar#isLenient()
	 */
	private static Date parseDateWithLeniency(
			final String str, final Locale locale, final String[] parsePatterns, final boolean lenient)
			throws ParseException {
		if (str == null || parsePatterns == null) {
			throw new IllegalArgumentException("Date and Patterns must not be null");
		}

		final TimeZone tz = TimeZone.getDefault();
		final Locale lcl = locale == null ? Locale.getDefault() : locale;
		final ParsePosition pos = new ParsePosition(0);
		final Calendar calendar = Calendar.getInstance(tz, lcl);
		calendar.setLenient(lenient);

		for (final String parsePattern : parsePatterns) {
			SimpleDateFormat sdf = new SimpleDateFormat(parsePattern, lcl);
			calendar.clear();
			sdf.setCalendar(calendar);
			try {
				Date date = sdf.parse(str, pos);
				if (date != null && pos.getIndex() == str.length()) {
					return calendar.getTime();
				}
			} catch (IllegalArgumentException ignore) {
				// leniency is preventing calendar from being set
			}
			pos.setIndex(0);
		}
		throw new ParseException("Unable to parse the date: " + str, -1);
	}

	//-----------------------------------------------------------------------
	/**
	 * Sets the years field to a date returning a new object.
	 * The original {@code Date} is unchanged.
	 *
	 * @param date  the date, not null
	 * @param amount the amount to set
	 * @return a new {@code Date} set with the specified value
	 * @throws IllegalArgumentException if the date is null
	 * @since 2.4
	 */
	public static Date setYears(final Date date, final int amount) {
		return set(date, Calendar.YEAR, amount);
	}

	//-----------------------------------------------------------------------
	/**
	 * Sets the months field to a date returning a new object.
	 * The original {@code Date} is unchanged.
	 *
	 * @param date  the date, not null
	 * @param amount the amount to set
	 * @return a new {@code Date} set with the specified value
	 * @throws IllegalArgumentException if the date is null
	 * @since 2.4
	 */
	public static Date setMonths(final Date date, final int amount) {
		return set(date, Calendar.MONTH, amount);
	}

	//-----------------------------------------------------------------------
	/**
	 * Sets the day of month field to a date returning a new object.
	 * The original {@code Date} is unchanged.
	 *
	 * @param date  the date, not null
	 * @param amount the amount to set
	 * @return a new {@code Date} set with the specified value
	 * @throws IllegalArgumentException if the date is null
	 * @since 2.4
	 */
	public static Date setDays(final Date date, final int amount) {
		return set(date, Calendar.DAY_OF_MONTH, amount);
	}

	//-----------------------------------------------------------------------
	/**
	 * Sets the milliseconds field to a date returning a new object.
	 * The original {@code Date} is unchanged.
	 *
	 * @param date  the date, not null
	 * @param amount the amount to set
	 * @return a new {@code Date} set with the specified value
	 * @throws IllegalArgumentException if the date is null
	 * @since 2.4
	 */
	public static Date setMilliseconds(final Date date, final int amount) {
		return set(date, Calendar.MILLISECOND, amount);
	}

	//-----------------------------------------------------------------------
	/**
	 * Sets the specified field to a date returning a new object.
	 * This does not use a lenient calendar.
	 * The original {@code Date} is unchanged.
	 *
	 * @param date  the date, not null
	 * @param calendarField  the {@code Calendar} field to set the amount to
	 * @param amount the amount to set
	 * @return a new {@code Date} set with the specified value
	 * @throws IllegalArgumentException if the date is null
	 * @since 2.4
	 */
	private static Date set(final Date date, final int calendarField, final int amount) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		// getInstance() returns a new object, so this method is thread safe.
		final Calendar c = Calendar.getInstance();
		c.setLenient(false);
		c.setTime(date);
		c.set(calendarField, amount);
		return c.getTime();
	}

	//-----------------------------------------------------------------------
	/**
	 * <p>Truncates a date, leaving the field specified as the most
	 * significant field.</p>
	 *
	 * <p>For example, if you had the date-time of 28 Mar 2002
	 * 13:45:01.231, if you passed with HOUR, it would return 28 Mar
	 * 2002 13:00:00.000.  If this was passed with MONTH, it would
	 * return 1 Mar 2002 0:00:00.000.</p>
	 *
	 * @param date  the date to work with, not null
	 * @param field  the field from {@code Calendar} or <code>SEMI_MONTH</code>
	 * @return the different truncated date, not null
	 * @throws IllegalArgumentException if the date is <code>null</code>
	 * @throws ArithmeticException if the year is over 280 million
	 */
	public static Date truncate(final Date date, final int field) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		final Calendar gval = Calendar.getInstance();
		gval.setTime(date);
		modify(gval, field, ModifyType.TRUNCATE);
		return gval.getTime();
	}

	//-----------------------------------------------------------------------
	/**
	 * <p>Internal calculation method.</p>
	 *
	 * @param val  the calendar, not null
	 * @param field  the field constant
	 * @param modType  type to truncate, round or ceiling
	 * @throws ArithmeticException if the year is over 280 million
	 */
	private static void modify(final Calendar val, final int field, final ModifyType modType) {
		if (val.get(Calendar.YEAR) > 280000000) {
			throw new ArithmeticException("Calendar value too large for accurate calculations");
		}

		if (field == Calendar.MILLISECOND) {
			return;
		}

		// ----------------- Fix for LANG-59 ---------------------- START ---------------
		// see http://issues.apache.org/jira/browse/LANG-59
		//
		// Manually truncate milliseconds, seconds and minutes, rather than using
		// Calendar methods.

		final Date date = val.getTime();
		long time = date.getTime();
		boolean done = false;

		// truncate milliseconds
		final int millisecs = val.get(Calendar.MILLISECOND);
		if (ModifyType.TRUNCATE == modType || millisecs < 500) {
			time = time - millisecs;
		}
		if (field == Calendar.SECOND) {
			done = true;
		}

		// truncate seconds
		final int seconds = val.get(Calendar.SECOND);
		if (!done && (ModifyType.TRUNCATE == modType || seconds < 30)) {
			time = time - seconds * 1000L;
		}
		if (field == Calendar.MINUTE) {
			done = true;
		}

		// truncate minutes
		final int minutes = val.get(Calendar.MINUTE);
		if (!done && (ModifyType.TRUNCATE == modType || minutes < 30)) {
			time = time - minutes * 60000L;
		}

		// reset time
		if (date.getTime() != time) {
			date.setTime(time);
			val.setTime(date);
		}
		// ----------------- Fix for LANG-59 ----------------------- END ----------------

		boolean roundUp = false;
		for (final int[] aField : fields) {
			for (final int element : aField) {
				if (element == field) {
					//This is our field... we stop looping
					if (modType == ModifyType.CEILING || modType == ModifyType.ROUND && roundUp) {
						if (field == DateUtils.SEMI_MONTH) {
							//This is a special case that's hard to generalize
							//If the date is 1, we round up to 16, otherwise
							//  we subtract 15 days and add 1 month
							if (val.get(Calendar.DATE) == 1) {
								val.add(Calendar.DATE, 15);
							} else {
								val.add(Calendar.DATE, -15);
								val.add(Calendar.MONTH, 1);
							}
							// ----------------- Fix for LANG-440 ---------------------- START ---------------
						} else if (field == Calendar.AM_PM) {
							// This is a special case
							// If the time is 0, we round up to 12, otherwise
							//  we subtract 12 hours and add 1 day
							if (val.get(Calendar.HOUR_OF_DAY) == 0) {
								val.add(Calendar.HOUR_OF_DAY, 12);
							} else {
								val.add(Calendar.HOUR_OF_DAY, -12);
								val.add(Calendar.DATE, 1);
							}
							// ----------------- Fix for LANG-440 ---------------------- END ---------------
						} else {
							//We need at add one to this field since the
							//  last number causes us to round up
							val.add(aField[0], 1);
						}
					}
					return;
				}
			}
			//We have various fields that are not easy roundings
			int offset = 0;
			boolean offsetSet = false;
			//These are special types of fields that require different rounding rules
			switch (field) {
			case DateUtils.SEMI_MONTH:
				if (aField[0] == Calendar.DATE) {
					//If we're going to drop the DATE field's value,
					//  we want to do this our own way.
					//We need to subtrace 1 since the date has a minimum of 1
					offset = val.get(Calendar.DATE) - 1;
					//If we're above 15 days adjustment, that means we're in the
					//  bottom half of the month and should stay accordingly.
					if (offset >= 15) {
						offset -= 15;
					}
					//Record whether we're in the top or bottom half of that range
					roundUp = offset > 7;
					offsetSet = true;
				}
				break;
			case Calendar.AM_PM:
				if (aField[0] == Calendar.HOUR_OF_DAY) {
					//If we're going to drop the HOUR field's value,
					//  we want to do this our own way.
					offset = val.get(Calendar.HOUR_OF_DAY);
					if (offset >= 12) {
						offset -= 12;
					}
					roundUp = offset >= 6;
					offsetSet = true;
				}
				break;
			default:
				break;
			}
			if (!offsetSet) {
				final int min = val.getActualMinimum(aField[0]);
				final int max = val.getActualMaximum(aField[0]);
				//Calculate the offset from the minimum allowed value
				offset = val.get(aField[0]) - min;
				//Set roundUp if this is more than half way between the minimum and maximum
				roundUp = offset > (max - min) / 2;
			}
			//We need to remove this field
			if (offset != 0) {
				val.set(aField[0], val.get(aField[0]) - offset);
			}
		}
		throw new IllegalArgumentException("The field " + field + " is not supported");

	}

}
