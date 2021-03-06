DROP TABLE IF EXISTS vevent;
CREATE TABLE vevent (
    dtstamp CHARACTER VARYING (150),
    uid CHARACTER VARYING (150),
    dtstart CHARACTER VARYING (150),
    class CHARACTER VARYING (150),
    created CHARACTER VARYING (150),
    description CHARACTER VARYING (150),
    geo CHARACTER VARYING (150),
    lastMod CHARACTER VARYING (150),
    location CHARACTER VARYING (150),
    organizer CHARACTER VARYING (150),
    priority CHARACTER VARYING (150),
    seq CHARACTER VARYING (150),
    status CHARACTER VARYING (150),
    summary CHARACTER VARYING (150),
    transp CHARACTER VARYING (150),
    url CHARACTER VARYING (150),
    recurid CHARACTER VARYING (150),
    rrule CHARACTER VARYING (150),
    dtend CHARACTER VARYING (150),
    duration CHARACTER VARYING (150),
    attach CHARACTER VARYING (150),
    attendee CHARACTER VARYING (150),
    categories CHARACTER VARYING (150),
    comment CHARACTER VARYING (150),
    contact CHARACTER VARYING (150),
    exdate CHARACTER VARYING (150),
    rstatus CHARACTER VARYING (150),
    related CHARACTER VARYING (150),
    resources CHARACTER VARYING (150),
    rdate CHARACTER VARYING (150),
    xProp CHARACTER VARYING (150),
    ianaProp CHARACTER VARYING (150),
    PRIMARY KEY (uid)
);
