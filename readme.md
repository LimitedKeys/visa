# VISA Dll Wrapper / Functions


## Resources 

- [PyVisa](https://github.com/pyvisa/pyvisa)
- [NI-VISA API Reference](https://www.ni.com/docs/en-US/bundle/ni-visa-api-ref/page/ni-visa-api-ref/viopendefaultrm.html)

## Open TODO

- [ ] Resource Info (pyvisa/highlevel.py::resource_info)
    - [ ] pyvisa/resources/resource.py::Resource.resource_info
- [ ] Open Resource (pyvisa/highlevel.py::open_resource)

It looks like "Open Resource" function works with multiple DLLs to make this all
work. So that's gonna be cool.

In short, it looks like VISA32 manages the session, and the other DLLs manage
instruments over the communication protocol.

PyVisa creates a bunch of access layer / class wrappers to help manage this. For
us, it may be a bit more tricky. We'll have to add some more DLLs to the build,
and (probably) start working on more wrappers...

- [ ] Dll.hs -> Dll/Visa.hs
- [ ] Dll/Usb.hs
- [ ] Dll/Gpib.hs

