import data from './extended.json' with { type: 'json5' };
report(data.name);
report(data.value);
exit(0);
