package net.bytebuddy.agent.builder;

import net.bytebuddy.test.utility.ObjectPropertyAssertion;
import org.junit.Test;

import java.io.PrintStream;
import java.util.*;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.*;

public class AgentBuilderRedefinitionStrategyListenerTest {

    @Test
    public void testNoOp() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.NoOp.INSTANCE.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.NoOp.INSTANCE.onError(0, Collections.<Class<?>>emptyList(), new Throwable(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.NoOp.INSTANCE.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
    }

    @Test
    public void testYielding() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.Yielding.INSTANCE.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.Yielding.INSTANCE.onBatch(1, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.Yielding.INSTANCE.onError(0, Collections.<Class<?>>emptyList(), new Throwable(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.Yielding.INSTANCE.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
    }

    @Test
    public void testPausing() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener listener = new AgentBuilder.RedefinitionStrategy.Listener.Pausing(1L);
        listener.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        listener.onBatch(1, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        listener.onError(0, Collections.<Class<?>>emptyList(), new Throwable(), Collections.<Class<?>>emptyList());
        listener.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
    }

    @Test
    public void testStreamWriting() throws Exception {
        PrintStream printStream = mock(PrintStream.class);
        AgentBuilder.RedefinitionStrategy.Listener listener = new AgentBuilder.RedefinitionStrategy.Listener.StreamWriting(printStream);
        listener.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        Throwable throwable = mock(Throwable.class);
        listener.onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList());
        listener.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
        verify(printStream, times(3)).printf(any(String.class), anyInt(), anyInt(), anyInt());
        verifyNoMoreInteractions(printStream);
        verify(throwable).printStackTrace(printStream);
        verifyNoMoreInteractions(throwable);
    }

    @Test
    public void testStreamWritingFactories() throws Exception {
        assertThat(AgentBuilder.RedefinitionStrategy.Listener.StreamWriting.toSystemOut(),
                is((AgentBuilder.RedefinitionStrategy.Listener) new AgentBuilder.RedefinitionStrategy.Listener.StreamWriting(System.out)));
        assertThat(AgentBuilder.RedefinitionStrategy.Listener.StreamWriting.toSystemError(),
                is((AgentBuilder.RedefinitionStrategy.Listener) new AgentBuilder.RedefinitionStrategy.Listener.StreamWriting(System.err)));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCompound() throws Exception {
        Throwable throwable = new Throwable();
        AgentBuilder.RedefinitionStrategy.Listener first = mock(AgentBuilder.RedefinitionStrategy.Listener.class), second = mock(AgentBuilder.RedefinitionStrategy.Listener.class);
        when(first.onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList())).thenReturn((Iterable) Collections.singleton(Collections.singletonList(Object.class)));
        when(second.onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList())).thenReturn((Iterable) Collections.singleton(Collections.singletonList(Void.class)));
        AgentBuilder.RedefinitionStrategy.Listener listener = new AgentBuilder.RedefinitionStrategy.Listener.Compound(first, second);
        listener.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        Iterator<? extends List<Class<?>>> batched = listener.onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList()).iterator();
        assertThat(batched.hasNext(), is(true));
        assertThat(batched.next(), is(Collections.<Class<?>>singletonList(Object.class)));
        assertThat(batched.hasNext(), is(true));
        assertThat(batched.next(), is(Collections.<Class<?>>singletonList(Void.class)));
        assertThat(batched.hasNext(), is(false));
        listener.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
        verify(first).onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        verify(first).onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList());
        verify(first).onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
        verifyNoMoreInteractions(first);
        verify(second).onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        verify(second).onError(0, Collections.<Class<?>>emptyList(), throwable, Collections.<Class<?>>emptyList());
        verify(second).onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
        verifyNoMoreInteractions(second);
    }

    @Test(expected = IllegalStateException.class)
    public void testErrorFailFast() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_FAST.onError(0,
                Collections.<Class<?>>emptyList(),
                mock(Throwable.class),
                Collections.<Class<?>>emptyList());
    }

    @Test
    public void testFailFastNoOp() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_FAST.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_FAST.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
    }

    @Test(expected = IllegalStateException.class)
    public void testErrorFailLast() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_LAST.onComplete(0,
                Collections.<Class<?>>emptyList(),
                Collections.singletonMap(Collections.<Class<?>>emptyList(), mock(Throwable.class)));
    }

    @Test
    public void testFailLastNoOp() throws Exception {
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_LAST.onBatch(0, Collections.<Class<?>>emptyList(), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_LAST.onError(0, Collections.<Class<?>>emptyList(), mock(Throwable.class), Collections.<Class<?>>emptyList());
        AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.FAIL_LAST.onComplete(0, Collections.<Class<?>>emptyList(), Collections.<List<Class<?>>, Throwable>emptyMap());
    }

    @Test(expected = NoSuchElementException.class)
    public void testEmptyIterator() throws Exception {
        new AgentBuilder.RedefinitionStrategy.Listener.Compound.CompoundIterable.CompoundIterator(Collections.<Iterable<? extends List<Class<?>>>>emptyList()).next();
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testIteratorRemove() throws Exception {
        new AgentBuilder.RedefinitionStrategy.Listener.Compound.CompoundIterable.CompoundIterator(Collections.<Iterable<? extends List<Class<?>>>>emptyList()).remove();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testBatchReallocatorNonBatchable() throws Exception {
        AgentBuilder.RedefinitionStrategy.BatchAllocator delegate = mock(AgentBuilder.RedefinitionStrategy.BatchAllocator.class);
        AgentBuilder.RedefinitionStrategy.Listener listener = new AgentBuilder.RedefinitionStrategy.Listener.BatchReallocator(delegate);
        assertThat(listener.onError(0, Collections.<Class<?>>singletonList(Object.class), new Throwable(), Collections.<Class<?>>emptyList()), is((Iterable) Collections.emptyList()));
        verifyZeroInteractions(delegate);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testBatchReallocatorBatchable() throws Exception {
        AgentBuilder.RedefinitionStrategy.BatchAllocator delegate = mock(AgentBuilder.RedefinitionStrategy.BatchAllocator.class);
        when(delegate.batch(Arrays.<Class<?>>asList(Object.class, Void.class))).thenReturn((Iterable) Collections.emptyList());
        AgentBuilder.RedefinitionStrategy.Listener listener = new AgentBuilder.RedefinitionStrategy.Listener.BatchReallocator(delegate);
        assertThat(listener.onError(0, Arrays.asList(Object.class, Void.class), new Throwable(), Collections.<Class<?>>emptyList()), is((Iterable) Collections.emptyList()));
        verify(delegate).batch(Arrays.asList(Object.class, Void.class));
        verifyNoMoreInteractions(delegate);
    }

    @Test
    public void testSplittingBatchReallocator() throws Exception {
        assertThat(AgentBuilder.RedefinitionStrategy.Listener.BatchReallocator.splitting(),
                is((AgentBuilder.RedefinitionStrategy.Listener) new AgentBuilder.RedefinitionStrategy.Listener.BatchReallocator(
                        new AgentBuilder.RedefinitionStrategy.BatchAllocator.Partitioning(2))));
    }

    @Test
    public void testObjectProperties() throws Exception {
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.NoOp.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.Compound.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.Compound.CompoundIterable.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.Compound.CompoundIterable.CompoundIterator.class).create(new ObjectPropertyAssertion.Creator<List<?>>() {
            @Override
            public List<?> create() {
                return Collections.emptyList();
            }
        }).applyBasic();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.Pausing.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.StreamWriting.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.Yielding.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.ErrorEscalating.class).apply();
        ObjectPropertyAssertion.of(AgentBuilder.RedefinitionStrategy.Listener.BatchReallocator.class).apply();
    }
}
