/** A ['a t] represents a circular queue (ordered collection) of ['a], with some
    element being the "current" in the queue. */

type t('a);

/** Create a circular queue with given element as the "current", and
    elements from the given list, while preserving their order. */

let create: ('a, list('a)) => t('a);

/** Return the "current" element in this queue */

let get_current: t('a) => 'a;

/** Rotate "current" element to the next one in the queue */

let rotate: t('a) => t('a);

/** Return an updated queue with current element removed.
    return [None] if the queue becomes empty after removal. */

let remove_current: t('a) => option(t('a));

/** Return a list where the first element is the "current" element in the queue,
    and the rest are in order */

let to_list: t('a) => list('a);
