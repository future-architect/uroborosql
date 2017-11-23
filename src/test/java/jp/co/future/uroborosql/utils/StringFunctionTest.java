package jp.co.future.uroborosql.utils;

import static org.junit.Assert.*;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Member;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

import ognl.MemberAccess;
import ognl.Ognl;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class StringFunctionTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	public void test() throws Exception {
		Map context = Ognl.createDefaultContext(new HashMap<Object, Object>(), new DefaultMemberAccess(false));
		Map root = Ognl.createDefaultContext(new HashMap<Object, Object>(), new DefaultMemberAccess(false));
		root.put("val1", null);
		root.put(StringFunction.SHORT_NAME, new StringFunction());

		Ognl.parseExpression("SF.isEmpty(val1)");

		assertTrue((boolean) Ognl.getValue("SF.isEmpty(val1)", context, root, null));
	}

	public static class DefaultMemberAccess implements MemberAccess {

		private boolean allowPrivateAccess = false;
		private boolean allowProtectedAccess = false;
		private boolean allowPackageProtectedAccess = false;

		public DefaultMemberAccess(final boolean allowAllAccess) {
			this(allowAllAccess, allowAllAccess, allowAllAccess);
		}

		public DefaultMemberAccess(final boolean allowPrivateAccess, final boolean allowProtectedAccess,
				final boolean allowPackageProtectedAccess) {
			super();
			this.allowPrivateAccess = allowPrivateAccess;
			this.allowProtectedAccess = allowProtectedAccess;
			this.allowPackageProtectedAccess = allowPackageProtectedAccess;
		}

		@SuppressWarnings("rawtypes")
		@Override
		public Object setup(final Map context, final Object target, final Member member, final String propertyName) {
			Object result = null;

			if (isAccessible(context, target, member, propertyName)) {
				AccessibleObject accessible = (AccessibleObject) member;

				if (!accessible.isAccessible()) {
					result = Boolean.TRUE;
					accessible.setAccessible(true);
				}
			}
			return result;
		}

		@SuppressWarnings("rawtypes")
		@Override
		public void restore(final Map context, final Object target, final Member member, final String propertyName,
				final Object state) {
			if (state != null) {
				((AccessibleObject) member).setAccessible(((Boolean) state).booleanValue());
			}
		}

		@SuppressWarnings("rawtypes")
		@Override
		public boolean isAccessible(final Map context, final Object target, final Member member,
				final String propertyName) {
			int modifiers = member.getModifiers();
			boolean result = Modifier.isPublic(modifiers);

			if (!result) {
				if (Modifier.isPrivate(modifiers)) {
					result = allowPrivateAccess;
				} else {
					if (Modifier.isProtected(modifiers)) {
						result = allowProtectedAccess;
					} else {
						result = allowPackageProtectedAccess;
					}
				}
			}
			return result;
		}
	}

}
