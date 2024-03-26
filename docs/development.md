# Development Notes

Notes taken during development / reverse engineering of PyVisa

## Open Notes

- [X] Resource Info (pyvisa/highlevel.py::resource_info)
    - [X] pyvisa/resources/resource.py::Resource.resource_info
- [X] Open Resource (pyvisa/highlevel.py::open_resource)
    - [X] viOpen
    - [X] open resource based on interface type?

There are specific resource management considerations based on the interface
type. The resource is opened the same way for all devices, but tha management
class in PyVisa changes.

This makes some sense for PyVisa - it makes accessing a device the same for USB
/ GPIB / TCPIP / etc.

We're interested in two use cases today:

- USB
- GPIB

I'm not sure if the USB GPIB is a USB or a GPIB device, but we can start with
USB and check it out tomorrow when (if) I get the chanse to play with it.

Both USB and GPIB devices are "MessageBasedResources" - which have some SUPER
COOL sauce, a `query` command. This, with the `write` command make the GPIB
driver stuff I like to do really work.

### Query / Write

APIs from (pyvisa/resources/messagebased.py):

- [ ] Get / Set termination
- [ ] write
- [ ] write_ascii_values
- [ ] write_binary_values
- [ ] read_bytes
- [ ] read_raw
- [ ] read
- [ ] read_ascii_values
- [ ] read_binary_values
- [ ] query
- [ ] query_ascii_values
- [ ] query_binary_values

Questions:

- What are these other functions for?

The NI Functions are:

- `viRead`
- `viWrite`

Termination should be carefully managed here. I bet every device has a slightly
different opinion on termination / etc. Especially older devices.


